module LibPunchLan.CodeGen.NasmCodegenerator

#nowarn "9"
#nowarn "51"

open System
open System.Text
open LibPunchLan
open LibPunchLan.Comp
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking
open LibPunchLan.TypeChecking.TypeChecker
open LibPunchLan.TypeChecking.TypeCheckerM
open LibPunchLan.Parsing
open LibPunchLan.Lexing
open System.IO
open FSharp.Reflection
open System.Security.Cryptography

type DeferBody =
    { Body: StringBuilder
      Offset: int }

type StackAllocator () =
    let mutable totalBytesAllocated = 0
    let mutable labelCount = 0

    let mutable defers = []

    member _.TotalBytesAllocated = totalBytesAllocated

    member _.Defers = defers

    member _.AllocateVar (name: string) (size: int) (env: Map<string, int>) =
        totalBytesAllocated <- totalBytesAllocated + size
        assert (((int size) % 8) = 0)
        assert (((int totalBytesAllocated) % 8) = 0)
        Map.add name -totalBytesAllocated env

    member _.AllocateAnonymousVar (size: int) =
            totalBytesAllocated <- totalBytesAllocated + size
            -totalBytesAllocated

    member this.AllocateDefer () =
        let offset = this.AllocateAnonymousVar 8
        let body = StringBuilder 8192
        let defer = { DeferBody.Body = body; Offset = offset }
        defers <- defers @ [ defer ]
        defer

    member _.AllocateLabel suffix =
        let label = sprintf $"%%$lbl%d{labelCount}_%s{suffix}"
        labelCount <- labelCount + 1
        label

type NasmContext =
    { CurrentSource: Source
      CurrentFunction: Function
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeRef>>
      StackEnv: Map<string, int>
      Allocator: StackAllocator
      StringBuilder: StringBuilder }

    member this.WithSource () =
        { SourceContext.CurrentSource = this.CurrentSource
          Program = this.Program
          NameTypeEnv = this.NameTypeEnv }

    member this.WithFunction () =
        { SourceFunctionContext.CurrentSource = this.CurrentSource
          CurrentFunction = this.CurrentFunction
          Program = this.Program
          NameTypeEnv = this.NameTypeEnv }

type IncomingArgInfo =
    { Index: int
      Name: string
      Type: TypeRef
      Size: int
      Offset: int }

type FuncCallInfo =
    { Args: IncomingArgInfo list
      ResultSize: int
      IsResultSizeFitsReg: bool
      IsFirstArgumentResultPtr: bool }


let bprintf format : TypeCheckerM.M<NasmContext, unit>  = tchecker {
    let! sb = getFromContext (fun c -> c.StringBuilder)
    Printf.bprintf sb format
}

let bprintfn format : TypeCheckerM.M<NasmContext, unit> = tchecker {
    let! sb = getFromContext (fun c -> c.StringBuilder)
    Printf.bprintf sb format
    Printf.bprintf sb "\n"
}

let sourceContext (m: TypeCheckerM.M<SourceContext, 'a>) : TypeCheckerM.M<NasmContext, 'a> = tchecker {
    let! context = getFromContext (fun c -> c.WithSource ())
    yield! checkWithContext context m
}

let sourceFuncContext (m: TypeCheckerM.M<SourceFunctionContext, 'a>) : TypeCheckerM.M<NasmContext, 'a> = tchecker {
    let! context = getFromContext (fun c -> c.WithFunction ())
    yield! checkWithContext context m
}

let getLabel (name: string) (source: Source) (modifier: Modifier option) =
    match modifier with
    | None ->
        let filename = source.Filename.Replace ('/', '_')
        sprintf $"%s{filename}___%s{name}"
    | Some Modifier.Export
    | Some Modifier.Extern -> name

let getLabel' name source = getLabel name source None

let getStringLabel (str: string) =
    let strBytes = Encoding.UTF8.GetBytes str
    let hashBytes = MD5.HashData strBytes
    let hex = Convert.ToHexString(hashBytes).Replace("-", "").ToLowerInvariant ()
    sprintf $"str_literal_%s{hex}"

let number2nasm (number: Number) =
    let sign = if NumberMod.getNumberSign number >= 0 then "" else "-"
    let number = NumberMod.unwrapNegative number
    match number with
    | Number.Integer decInts ->
        let str = decInts |> Array.map NumberMod.decIntToStr |> String.concat ""
        sprintf $"%s{sign}%s{str}d"
    | Number.HexInteger hexInts ->
        let str = hexInts |> Array.map NumberMod.hexIntToStr |> String.concat ""
        sprintf $"%s{sign}%s{str}h"
    | Number.BinaryInteger binInts ->
        let str = binInts |> Array.map NumberMod.bitIntToStr |> String.concat ""
        sprintf $"%s{sign}%s{str}b"
    | Number.Double dbl ->
        // Converts double to its hex representation
        assert (sizeof<double> = sizeof<uint64>)
        let mutable double: double = dbl
        let mutable pdouble: nativeint = NativeInterop.NativePtr.toNativeInt (&&double)
        let mutable long: uint64 = NativeInterop.NativePtr.ofNativeInt<uint64> pdouble |> NativeInterop.NativePtr.read
        sprintf $"0%016x{long}h"
    | Number.Negative _ -> failwith "Negative number should have been removed at this point."

let string2nasm (str: string) =
    let bytes = Encoding.UTF8.GetBytes str
    let hex =
        bytes
        |> Seq.ofArray
        |> Seq.map (fun b -> sprintf $"0%x{b}h")
    let hex = Seq.append hex (Seq.init 8 (fun _ -> "00h"))
    String.concat ", " hex


let getField (typ: TypeDeclRef) (name: string) : TypeCheckerM.M<NasmContext, FieldInfo> = tchecker {
    let! fields = sourceContext (getTypeDeclFields typ)
    match fields |> List.tryFind (fun f -> f.Name = name) with
    | Some field -> yield field
    | None -> yield! sourceContext (fatalDiag $"Can't find field namde '%s{name}' in '%O{typ}'")
}

let getFuncCallInfo (func: Function) : TypeCheckerM.M<SourceContext, FuncCallInfo>  = tchecker {
    let! source = getFromContext (fun c -> c.CurrentSource)
    let! argumentSizes =
        func.Args
        |> List.map snd
        |> List.map (fun t -> getTypeIdSize { TypeId = t; Source = source })
        |> unwrapList

    let! returnSize = getTypeIdSize { TypeId = func.ReturnType; Source = source }
    let returnSizeFitsReg = 1 <= returnSize && returnSize <= 8
    let isFirstArgResultPtr = not (TypeId.isVoid func.ReturnType) && not returnSizeFitsReg
    let initialOffset = if isFirstArgResultPtr then 24 else 16

    let argumentInfo =
        List.indexed (List.zip func.Args argumentSizes)
        |> List.map (fun (index, ((argName, argType), argSize)) ->
            { IncomingArgInfo.Index = index; Name = argName; Type = { TypeId = argType; Source = source }
              Size = argSize; Offset = 0 })

    let _, argumentInfo =
        argumentInfo
        |> List.fold (fun (offset, args) argInfo ->
            let args = { argInfo with Offset = offset } :: args
            let offset = offset + (align argInfo.Size 8)
            (offset, args)) (initialOffset, [])

    yield { FuncCallInfo.Args = argumentInfo
            ResultSize = returnSize
            IsFirstArgumentResultPtr = isFirstArgResultPtr
            IsResultSizeFitsReg = returnSizeFitsReg }
}

/// Copy from address (located at top of stack) to stack
let writeCopyToStack (typ: TypeRef) : TypeCheckerM.M<NasmContext, unit> = tchecker {
    do! bprintfn "pop rax"
    match TypeId.unwrapConst typ.TypeId with
    | TypeId.Uint8
    | TypeId.Char ->
        do! bprintfn "movzx rax, byte [rax]"
        do! bprintfn "push rax"
    | TypeId.Uint16 ->
        do! bprintfn "movzx rax, word [rax]"
        do! bprintfn "push rax"
    | TypeId.Uint32 ->
        do! bprintfn "mov rbx, 0"
        do! bprintfn "mov ebx, dword [rax]"
        do! bprintfn "push rbx"
    | TypeId.Uint64 ->
        do! bprintfn "mov rax, qword [rax]"
        do! bprintfn "push rax"
    | TypeId.Int8 ->
        do! bprintfn "movsx rax, byte [rax]"
        do! bprintfn "push rax"
    | TypeId.Int16 ->
        do! bprintfn "movsx rax, word [rax]"
        do! bprintfn "push rax"
    | TypeId.Int32 ->
        do! bprintfn "movsx rax, dword [rax]"
        do! bprintfn "push rax"
    | TypeId.Int64
    | TypeId.Bool
    | TypeId.Pointer _ ->
        do! bprintfn "mov rax, qword [rax]"
        do! bprintfn "push rax"
    | TypeId.Void -> yield! sourceFuncContext (fatalDiag' "Can't copy to stack void type.")
    | TypeId.Float ->
        do! bprintfn "fld dword [rax]"
        do! bprintfn "sub rsp, 8"
        do! bprintfn "fstp qword [rsp]"
    | TypeId.Double ->
        do! bprintfn "mov rax, qword [rax]"
        do! bprintfn "push rax"
    | TypeId.Const _  as typ -> yield! sourceFuncContext (fatalDiag' $"Const(%O{typ}) should have been removed at this point.")
    | TypeId.Named _  ->
        let! size = sourceContext (getTypeIdSize typ)
        do! bprintfn $"sub rsp, %d{align size 8}"
        do! bprintfn $"push %d{size}"
        do! bprintfn "push rsp"
        do! bprintfn "push rax"
        do! bprintfn "memcopy"
}

/// Copy from stack to address (which is located on top of stack)
let writeCopyFromStack (typ: TypeRef) : TypeCheckerM.M<NasmContext, unit> = tchecker {
    do! bprintfn "pop rax"
    match TypeId.unwrapConst typ.TypeId with
    | TypeId.Char
    | TypeId.Int8
    | TypeId.Uint8 ->
        do! bprintfn "pop rbx"
        do! bprintfn "mov byte [rax], bl"
    | TypeId.Int16
    | TypeId.Uint16 ->
        do! bprintfn "pop rbx"
        do! bprintfn "mov word [rax], bx"
    | TypeId.Int32
    | TypeId.Uint32
    | TypeId.Float ->
        do! bprintfn "pop rbx"
        do! bprintfn "mov dword [rax], ebx"
    | TypeId.Int64
    | TypeId.Uint64
    | TypeId.Double
    | TypeId.Pointer _
    | TypeId.Bool ->
        do! bprintfn "pop rbx"
        do! bprintfn "mov qword [rax], rbx"
    | TypeId.Void -> yield! sourceFuncContext (fatalDiag' "Type 'void' cannot be copied from stack.")
    | TypeId.Named _ ->
        let! size = sourceContext (getTypeIdSize typ)
        do! bprintfn $"push %d{size}"
        do! bprintfn "push rax"
        do! bprintfn "push rsp"
        do! bprintfn "memcopy"
        do! bprintfn $"add rsp, %d{align size 8}"
    | typ -> failwithf $"Type '%O{typ}' should have been covered"
}

let nasmTrueConst = "0ffffffffffffffffh"
let nasmFalseConst = "00h"

type CallingConvention =
    | MicrosoftX64
    | SysVX64

type NasmCodegenerator (tw: TextWriter, program: Program, callconv: CallingConvention) =

    let fprintf text = Printf.fprintf tw text
    let fprintfn text =
        Printf.fprintf tw text
        Printf.fprintf tw "\n"


    let rec writeExpression (expression: Expression) : TypeCheckerM.M<NasmContext, unit>  = tchecker {
        let! ssourceContext = getFromContext (fun c -> c.WithSource ())

        match expression with

        | Expression.Constant (Value.String str) ->
            let stringLabel = getStringLabel str
            do! bprintfn $"mov rax, %s{stringLabel}"
            do! bprintfn "push rax"

        | Expression.Constant (Value.Boolean bool) ->
            let boolStr =
                match bool with
                | true -> nasmTrueConst
                | false -> nasmFalseConst
            do! bprintfn $"push %s{boolStr}"

        | Expression.Constant (Value.Char char) ->
            do! bprintfn $"push %x{int char} ; Char '%c{char}'"

        | Expression.Constant (Value.Number number) ->
            let number = number2nasm number
            do! bprintfn $"mov rax, %s{number}"
            do! bprintfn "push rax"

        | Expression.Variable _ as expression ->
            let! exprType = sourceContext (getExpressionType expression)
            do! writeExpressionAddress expression
            do! writeCopyToStack exprType

        | Expression.FuncCall (name, args) ->
            let! func = sourceContext (locateFunctionDecl name)
            let! funcReturnSize = sourceContext (getTypeIdSize { TypeId = func.Function.ReturnType; Source = func.Source })
            let funcReturnSizeFitsReg = 1 <= funcReturnSize && funcReturnSize <= 8
            let! allocator = getFromContext (fun c -> c.Allocator)
            let resultPointerRequired = not (TypeId.isVoid func.Function.ReturnType) && not funcReturnSizeFitsReg

            match func.Function.Modifier with
            | None
            | Some Modifier.Export
            | Some Modifier.Extern ->
                for expression in List.rev args do
                    do! writeExpression expression

                let returnValuePtr = if resultPointerRequired
                                     then allocator.AllocateAnonymousVar (align funcReturnSize 8)
                                     else 0

                if resultPointerRequired then
                    do! bprintfn $"lea rax, qword [rbp-%d{returnValuePtr}]"
                    do! bprintfn "push rax"

                let label = getLabel' name.Name func.Source
                do! bprintfn $"call %s{label}"

                match TypeId.isVoid func.Function.ReturnType with
                | true -> ()
                | false when funcReturnSizeFitsReg ->
                    do! bprintfn "push rax"
                | false ->
                    do! bprintfn $"sub rsp, %d{align funcReturnSize 8}"
                    do! bprintfn $"push %d{funcReturnSize}"
                    do! bprintfn "push rsp"
                    do! bprintfn $"lea rax, qword [rbp%+d{returnValuePtr}]"
                    do! bprintfn "push rax"
                    do! bprintfn "memcopy"

        | Expression.MemberAccess (Expression.Variable variableName as leftExpr, memberName)
            when isStructVariableWithMember variableName memberName ssourceContext ->
            let! typ = sourceContext (getExpressionType leftExpr)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldInfo = getField typeDecl memberName
                do! writeExpressionAddress leftExpr
                do! bprintfn "pop rax"
                do! bprintfn $"add rax, %d{fieldInfo.Offset} ; Offset to '%O{typeDecl.TypeDecl.Name}'.'%s{memberName}'"
                do! bprintfn "push rax"
                do! writeCopyToStack fieldInfo.Type
            | typ -> failwithf $"'%O{typ}' should have been already covered."

        | Expression.MemberAccess (Expression.Variable alias, name) when getAliasedSource alias ssourceContext |> Result.isOk ->
            let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = Some alias })
            let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
            do! bprintfn $"mov rax, qword %s{label}"
            do! bprintfn "push rax"
            do! writeCopyToStack { TypeId = varDecl.Variable.TypeId; Source = varDecl.Source }

        | Expression.MemberAccess (left, name) ->
            let! typ = sourceContext (getExpressionType left)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldInfo = getField typeDecl name
                do! writeExpressionAddress left
                do! bprintfn "pop rax"
                do! bprintfn $"add rax, %d{fieldInfo.Offset} ; Offset to '%O{typeDecl.TypeDecl.Name}'.'%s{name}'"
                do! bprintfn "push rax"
                do! writeCopyToStack fieldInfo.Type
            | typ -> yield! sourceFuncContext (fatalDiag' $"Member access to '%O{typ}'.%s{name} is not supported.")

        | Expression.BinaryExpression binExpr -> do! writeBinaryExpression binExpr

        | Expression.ArrayAccess (left, indexExpr) ->
            let! arrayType = sourceContext (getExpressionType left)
            let! arraySubitemType = sourceContext (getArraySubitemType arrayType.TypeId)
            let! size = sourceContext (getTypeIdSize { TypeId = arraySubitemType; Source = arrayType.Source })

            do! writeExpressionAddress left
            do! writeExpression indexExpr
            do! bprintfn "pop rax"
            do! bprintfn "pop rbx"
            do! bprintfn $"mov rcx, %+d{size}"
            do! bprintfn "mul rcx"
            do! bprintfn "lea rax, qword [rbx+rax]"
            do! bprintfn "push rax"
            do! writeCopyToStack { TypeId = arraySubitemType; Source = arrayType.Source }

        | Expression.StructCreation (name, fields) as expr ->
            let! size = sourceContext (getExpressionSize expr)
            let! typeDecl = sourceContext (locateTypeDecl name)

            do! bprintfn $"sub rsp, %d{align size 8}"
            for fieldName, fieldExpr in fields do
                let! fieldInfo = getField typeDecl fieldName
                do! writeExpression fieldExpr
                do! bprintfn $"lea rax, qword [rsp+%d{fieldInfo.Offset}]"
                do! bprintfn "push rax"
                do! writeCopyFromStack fieldInfo.Type

        | Expression.Bininversion expr ->
            do! writeExpression expr
            do! bprintfn "pop rax"
            do! bprintfn "not rax"
            do! bprintfn "push rax"

        | Expression.Sizeof typeId ->
            let! size = sourceContext (getTypeIdSize { TypeId = typeId; Source = ssourceContext.CurrentSource })
            do! bprintfn $"mov rax, %d{size}d"
            do! bprintfn "push rax"

        | Expression.Addrof expr -> do! writeExpressionAddress expr

        | Expression.Deref expr ->
            let! typeId = sourceContext (getExpressionType expr)
            match TypeId.unwrapConst typeId.TypeId with
            | TypeId.Pointer subtype ->
                do! writeExpression expr
                do! writeCopyToStack { TypeId = subtype; Source = typeId.Source }
            | _ -> yield! sourceContext (fatalDiag "Only expression of type 'pointer<>' can be used as an argument for 'deref' function.")

        | Expression.Cast (targetType, sourceExpression) ->
            let! sourceType = sourceContext (getExpressionType sourceExpression)
            let destinationType = { TypeId = targetType; Source = ssourceContext.CurrentSource }
            match TypeId.isTypesEqual sourceType destinationType with
            | true -> do! writeExpression sourceExpression
            | false ->
                let left = sourceType.TypeId
                let right = destinationType.TypeId

                if (TypeId.isSigned left && TypeId.isUnsigned right) ||
                   (TypeId.isUnsigned left && TypeId.isSigned right) then

                    do! writeExpression sourceExpression

                elif (TypeId.isSigned left || TypeId.isUnsigned right) &&
                     TypeId.isFloat right then

                    do! writeExpression sourceExpression
                    do! bprintfn "fild qword [rsp]"
                    do! bprintfn "mov rax, 0"
                    do! bprintfn "mov qword [rsp], rax"
                    do! bprintfn "fstp qword [rsp]"

                elif TypeId.isFloat left &&
                     (TypeId.isSigned right || TypeId.isUnsigned right) then

                    do! writeExpression sourceExpression
                    do! bprintfn "fld qword [rsp]"
                    do! bprintfn "mov rax, 0"
                    do! bprintfn "mov qword [rsp], rax"
                    do! bprintfn "fistp qword [rsp]"

                elif TypeId.isPointer left && TypeId.isPointer right then
                    do! writeExpression sourceExpression

                else
                    yield! sourceContext (diag $"Cast from '%O{sourceType}' to '%O{destinationType}' should have been covered.")

    }

    and writeBinaryExpression (expression: BinaryExpression) : TypeCheckerM.M<NasmContext, unit> = tchecker {
        let left = expression.Left
        let right = expression.Right
        let! leftType = sourceContext (getExpressionType left)
        let! rightType = sourceContext (getExpressionType right)
        let leftType = leftType.TypeId
        let rightType = rightType.TypeId
        let! allocator = getFromContext (fun c -> c.Allocator)

        match expression.Kind with
        | BinaryExpressionKind.Plus ->

            do! writeExpression right
            do! writeExpression left

            if (TypeId.isPointer leftType || TypeId.isPointer rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "add rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "add rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "faddp"
                do! bprintfn "sub rsp, 8"
                do! bprintfn "fstp qword [rsp]"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' + '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Minus ->

            do! writeExpression right
            do! writeExpression left

            if (TypeId.isPointer leftType || TypeId.isPointer rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "sub rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "sub rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fsubp"
                do! bprintfn "sub rsp, 8"
                do! bprintfn "fstp qword [rsp]"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' - '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Multiply ->

            do! writeExpression right
            do! writeExpression left

            if TypeId.isSigned leftType && TypeId.isSigned rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "imul rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isUnsigned leftType && TypeId.isUnsigned rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "mul rbx"
                do! bprintfn "push rax"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fmulp"
                do! bprintfn "sub rsp, 8"
                do! bprintfn "fstp qword [rsp]"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType} * '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Division ->

            do! writeExpression right
            do! writeExpression left

            if TypeId.isSigned leftType && TypeId.isSigned rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "idiv rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isUnsigned leftType && TypeId.isUnsigned rightType then
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "div rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fdivp"
                do! bprintfn "sub rsp, 8"
                do! bprintfn "fstp qword [rsp]"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' / '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Equal ->
            let label1 = allocator.AllocateLabel "equal"
            let label2 = allocator.AllocateLabel "equal_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) ||
               (TypeId.isFloat leftType && TypeId.isFloat rightType)  ||
               (TypeId.isBool leftType && TypeId.isBool rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"je %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' == '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.NotEqual ->
            let label1 = allocator.AllocateLabel "not_equal"
            let label2 = allocator.AllocateLabel "not_equal_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) ||
               (TypeId.isBool leftType && TypeId.isBool rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"jne %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' != '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Less ->
            let label1 = allocator.AllocateLabel "less"
            let label2 = allocator.AllocateLabel "less_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"jb %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fcompp"
                do! bprintfn "mov rax, 0"
                do! bprintfn "fsctw al"
                do! bprintfn "cmp rax, 001b"
                do! bprintfn $"je %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' < '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.LessOrEqual ->
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "less_or_equal")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "less_or_equal_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"jbe %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fcompp"
                do! bprintfn "mov rax, 0"
                do! bprintfn "fsctw al"
                do! bprintfn "cmp rax, 001b"
                do! bprintfn $"je %s{label1}"
                do! bprintfn "cmp rax, 100b"
                do! bprintfn $"je %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' <= '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Greater ->
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) ||
                (TypeId.isIntegerType leftType || TypeId.isIntegerType rightType)) then

                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"ja %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fcompp"
                do! bprintfn "mov rax, 0"
                do! bprintfn "fsctw al"
                do! bprintfn "cmp rax, 000b"
                do! bprintfn $"je %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}: "

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' < '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.GreaterOrEqual ->
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_or_equal")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_or_equal_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointer leftType || TypeId.isPointer rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) ||
                (TypeId.isIntegerType leftType || TypeId.isIntegerType rightType)) then

                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "cmp rax, rbx"
                do! bprintfn $"jae %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then

                do! bprintfn "fld qword [rsp]"
                do! bprintfn "fld qword [rsp+8]"
                do! bprintfn "add rsp, 16"
                do! bprintfn "fcompp"
                do! bprintfn "mov rax, 0"
                do! bprintfn "fsctw al"
                do! bprintfn "cmp rax, 000b"
                do! bprintfn $"jmp %s{label1}"
                do! bprintfn "cmp rax, 100b"
                do! bprintfn $"jmp %s{label1}"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"jmp %s{label2}"
                do! bprintfn $"%s{label1}:"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"%s{label2}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' >= '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.Or ->
            let! rightBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_right_branch")
            let! endOfOrBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_end_of_branch")
            let! falseBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_false_branch")

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "or rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isBool leftType && TypeId.isBool rightType then
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "cmp rax, 0"
                do! bprintfn $"je %s{rightBranch}"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"jmp %s{endOfOrBranch}"
                do! bprintfn $"%s{rightBranch}:"
                do! writeExpression right
                do! bprintfn "pop rax"
                do! bprintfn "cmp rax, 0"
                do! bprintfn $"je %s{falseBranch}"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"jmp %s{endOfOrBranch}"
                do! bprintfn $"%s{falseBranch}:"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"%s{endOfOrBranch}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' or '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.And ->
            let! falseBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "and_false_branch")
            let! endOfAndBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "and_end_of_branch")

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "and rax, rbx"
                do! bprintfn "push rax"

            elif TypeId.isBool leftType && TypeId.isBool rightType then
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "cmp rax, 0"
                do! bprintfn $"je %s{falseBranch}"
                do! writeExpression right
                do! bprintfn "pop rax"
                do! bprintfn "cmp rax, 0"
                do! bprintfn $"je %s{falseBranch}"
                do! bprintfn $"push %s{nasmTrueConst}"
                do! bprintfn $"jmp %s{endOfAndBranch}"
                do! bprintfn $"%s{falseBranch}:"
                do! bprintfn $"push %s{nasmFalseConst}"
                do! bprintfn $"%s{endOfAndBranch}:"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' and '%O{rightType}' should have been covered")

        | BinaryExpressionKind.Xor ->

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "xor rax, rbx"
                do! bprintfn "push rax"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' xor '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.RShift ->

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "shr rax, rbx"
                do! bprintfn "push rax"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' >> '%O{rightType}' should have been covered.")

        | BinaryExpressionKind.LShift ->

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn "pop rax"
                do! bprintfn "pop rbx"
                do! bprintfn "shl rax, rbx"
                do! bprintfn "push rax"

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' << '%O{rightType}' should have been covered.")
    }

    and writeExpressionAddress (expression: Expression) : TypeCheckerM.M<NasmContext, unit> = tchecker {
        let! stackEnv = getFromContext (fun c -> c.StackEnv)
        let! ssourceContext = getFromContext (fun c -> c.WithSource ())

        match expression with
        | Expression.Variable name ->
            match Map.tryFind name stackEnv with
            | Some stackOffset ->
                do! bprintfn $"lea rax, qword [rbp%+d{stackOffset}]"
                do! bprintfn "push rax"
            | None ->
                let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = None })
                let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
                do! bprintfn $"mov rax, qword %s{label}"
                do! bprintfn "push rax"

        | Expression.MemberAccess (Expression.Variable varname as variable, memberName)
            when isStructVariableWithMember varname memberName ssourceContext ->
            let! variableType = sourceContext (getExpressionType variable)
            match TypeId.unwrapConst variableType.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = variableType.Source }) (locateTypeDecl typename)
                let! fieldInfo = getField typeDecl memberName
                do! writeExpressionAddress variable
                do! bprintfn "pop rax"
                do! bprintfn $"add rax, %d{fieldInfo.Offset} ; Offset to '%s{typeDecl.TypeDecl.Name}.'%s{memberName}'"
                do! bprintfn "push rax"
            | typ -> failwithf $"Should not happen. Type '%O{typ}' should have been covered by isStructVariableWithMember function."

        | Expression.MemberAccess (Expression.Variable alias, name)
            when getAliasedSource alias ssourceContext |> Result.isOk ->
            let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = Some alias })
            let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
            do! bprintfn $"mov rax, qword %s{label}"
            do! bprintfn "push rax"

        | Expression.MemberAccess (left, memberName) ->
            let! typ = sourceContext (getExpressionType left)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldInfo = getField typeDecl memberName
                do! writeExpressionAddress left
                do! bprintfn "pop rax"
                do! bprintfn $"add rax, %d{fieldInfo.Offset} ; Offset to '%s{typeDecl.TypeDecl.Name}.'%s{memberName}'"
                do! bprintfn "push rax"
            | typ -> yield! sourceFuncContext (fatalDiag' $"Member access '%O{typ}'.%s{memberName} is not supported.")

        | Expression.ArrayAccess (array, index) ->
            let! arrayType = sourceContext (getExpressionType array)
            let! arraySubitemType = sourceContext (getArraySubitemType arrayType.TypeId)
            let! size = sourceContext (getTypeIdSize { TypeId = arraySubitemType; Source = arrayType.Source })

            do! writeExpression array
            do! writeExpression index
            do! bprintfn "pop rax"
            do! bprintfn "pop rbx"
            do! bprintfn $"mov rcx, %+d{size}"
            do! bprintfn "mul rcx"
            do! bprintfn "lea rax, byte [rbx+rax]"
            do! bprintfn "push rax"

        | Expression.Deref expression -> do! writeExpression expression

        | expression ->
            let! typ = sourceContext (getExpressionType expression)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Pointer _ ->
                do! writeExpression expression
            | _ ->
                let unionCaseInfo, _ = FSharpValue.GetUnionFields (expression, expression.GetType ())
                yield! sourceFuncContext (fatalDiag' $"Can't get address of expression '%s{unionCaseInfo.Name}'.")
    }

    let rec writeStatement (statement: Statement) (endOfFunctionLabel: string)
                            : TypeCheckerM.M<NasmContext, NasmContext>
                                = tchecker {

        match statement with

        | Statement.VarDecl (name, varType, initExpr) ->
            let! source, allocator, stackEnv, nameTypeEnv = getFromContext (fun c -> (c.CurrentSource, c.Allocator, c.StackEnv, c.NameTypeEnv))
            let! varSize = sourceContext (getTypeIdSize { TypeId = varType; Source = source })
            let stackEnv = allocator.AllocateVar name (align varSize 8) stackEnv
            let nameTypeEnv = lazy (Map.add name { TypeId = varType; Source = source } nameTypeEnv.Value)
            let! context = getFromContext (fun c -> { c with NameTypeEnv = nameTypeEnv; StackEnv = stackEnv })

            match initExpr with
            | Some expression ->
                let! exprType = sourceContext (getExpressionType expression)
                do! writeExpression expression
                do! checkWithContext context (writeExpressionAddress (Expression.Variable name))
                do! writeCopyFromStack exprType
            | None -> ()

            yield context

        | Statement.VarAssignment (leftExpr, rightExpr) ->
            let! leftExprType = sourceContext (getExpressionType leftExpr)

            do! writeExpression rightExpr
            do! writeExpressionAddress leftExpr
            do! writeCopyFromStack leftExprType

            yield! context

        | Statement.If (mainCond, elseIfs, elses) ->
            let! allocator = getFromContext (fun c -> c.Allocator)
            let endOfIfBranch = allocator.AllocateLabel "end_of_if"
            let writeIfCond (ifCond: IfCond) elseBranch : TypeCheckerM.M<NasmContext, unit> = tchecker {
                do! writeExpression ifCond.Condition
                do! bprintfn "pop rax"
                do! bprintfn "cmp rax, 0"
                do! bprintfn $"je %s{elseBranch}"
                do! writeStatements ifCond.Body endOfFunctionLabel
                do! bprintfn $"jmp %s{endOfIfBranch}"
            }
            let folder (elseBranch, writer) ifCond =
                let nextElseBranch = allocator.AllocateLabel "else_if"
                let nextWriter = (fun () -> tchecker {
                    do! writer ()
                    do! bprintfn $"%s{elseBranch}:"
                    do! writeIfCond ifCond nextElseBranch
                })
                (nextElseBranch, nextWriter)

            let elseBranch = allocator.AllocateLabel "else_if"
            let finalElseBranch, writerThatWritesAllIfs =
                elseIfs |> List.fold folder (elseBranch, (fun () -> tchecker { yield () }))

            do! writeIfCond mainCond elseBranch
            do! writerThatWritesAllIfs ()
            do! bprintfn $"%s{finalElseBranch}:"
            do! writeStatements elses endOfFunctionLabel
            do! bprintfn $"%s{endOfIfBranch}:"

            yield! context

        | Statement.For (indexVariable, startExpression, endExpression, stepExpression, body) ->
            let! source, allocator, stackEnv, nameTypeEnv = getFromContext (fun c -> (c.CurrentSource, c.Allocator, c.StackEnv, c.NameTypeEnv))
            let newStackEnv = allocator.AllocateVar indexVariable 8 stackEnv
            let newNameTypeEnv = lazy (Map.add indexVariable { TypeId = TypeId.Int64; Source = source } nameTypeEnv.Value)
            let! newContext = getFromContext (fun c -> { c with NameTypeEnv = newNameTypeEnv; StackEnv = newStackEnv })
            let stepExpression = stepExpression |> Option.defaultValue (Expression.Constant (Value.Number (Number.Integer [| DecInt.One |])))

            do! writeExpression startExpression
            do! checkWithContext newContext (writeExpressionAddress (Expression.Variable indexVariable))
            do! checkWithContext newContext (writeCopyFromStack { TypeId = TypeId.Int64; Source = source })

            let compareIndexVarLabel = allocator.AllocateLabel "for_cmp_index_var"
            let endOfForLoopLabel = allocator.AllocateLabel "for_end"

            do! bprintfn $"%s{compareIndexVarLabel}:"
            do! checkWithContext newContext (writeExpression endExpression)
            do! checkWithContext newContext (writeExpression (Expression.Variable indexVariable))
            do! bprintfn "pop rax"
            do! bprintfn "pop rbx"
            do! bprintfn "cmp rax, rbx"
            do! bprintfn $"je %s{endOfForLoopLabel}"
            do! checkWithContext newContext (writeStatements body endOfFunctionLabel)
            do! checkWithContext newContext (writeExpression stepExpression)
            do! checkWithContext newContext (writeExpression (Expression.Variable indexVariable))
            do! bprintfn "pop rax"
            do! bprintfn "pop rbx"
            do! bprintfn "add rax, rbx"
            do! bprintfn "push rax"
            do! checkWithContext newContext (writeExpressionAddress (Expression.Variable indexVariable))
            do! checkWithContext newContext (writeCopyFromStack { TypeId = TypeId.Int64; Source = source })
            do! bprintfn $"jmp %s{compareIndexVarLabel}"
            do! bprintfn $"%s{endOfForLoopLabel}:"

            yield! context

        | Statement.While (condition, body) ->
            let! allocator = getFromContext (fun c -> c.Allocator)
            let startOfWhileLoop = allocator.AllocateLabel "while_start"
            let endOfWhileLoop = allocator.AllocateLabel "while_end"

            do! bprintfn $"%s{startOfWhileLoop}:"
            do! writeExpression condition
            do! bprintfn "pop rax"
            do! bprintfn "cmp rax, 0"
            do! bprintfn $"je %s{endOfWhileLoop}"
            do! writeStatements body endOfFunctionLabel
            do! bprintfn $"jmp %s{startOfWhileLoop}"
            do! bprintfn $"%s{endOfWhileLoop}:"

            yield! context

        | Statement.Defer deferStatements ->
            match deferStatements with
            | [] -> yield! context
            | deferStatements ->
                let! allocator = getFromContext (fun c -> c.Allocator)
                let defer = allocator.AllocateDefer ()

                do! bprintfn $"mov rax, %s{nasmTrueConst}"
                do! bprintfn $"mov qword [rbp%+d{defer.Offset}], rax ; Set defer to 'true'"
                let! newContext = getFromContext (fun c -> { c with StringBuilder = defer.Body })
                do! checkWithContext newContext (writeStatements deferStatements endOfFunctionLabel)
                yield! context

        | Statement.Return ->
            do! bprintfn $"jmp %s{endOfFunctionLabel}"
            yield! context

        | Statement.ReturnExpr expression ->
            do! writeExpression expression
            do! bprintfn $"jmp %s{endOfFunctionLabel}"
            yield! context

        | Statement.Expression (Expression.FuncCall (name, _) as expression) ->
            let! func = sourceContext (locateFunctionDecl name)
            let! size = sourceContext (getExpressionSize expression)

            do! writeExpression expression
            match TypeId.isVoid func.Function.ReturnType with
            | true -> ()
            | false when 1 <= size && size <= 8 ->
                do! bprintfn "mov rax, 0"
            | false -> ()

            yield! context

        | Statement.Expression expression ->
            let unionCaseInfo, _ = FSharpValue.GetUnionFields (expression, expression.GetType ())
            yield! sourceFuncContext (fatalDiag' $"Expression of type '%s{unionCaseInfo.Name}' is not supported as statement.")
    }

    and writeStatements (statements: Statement list) (endOfFunctionLabel: string) : TypeCheckerM.M<NasmContext, unit>
                         = tchecker {

        let folder writer statement = (fun () -> tchecker {
            let! context = writer ()
            yield! checkWithContext context (writeStatement statement endOfFunctionLabel)
        })

        let! context = context
        let bigWriter =
            statements
            |> List.fold folder (fun () -> tchecker { yield context })

        let! _ = bigWriter ()
        yield ()
    }


    let writeNativeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! context = context
        let! argumentsInfo = getFuncCallInfo func

        let stackEnv =
            argumentsInfo.Args
            |> List.fold (fun env (arg: IncomingArgInfo) -> Map.add arg.Name arg.Offset env) Map.empty

        let nameTypeEnv = lazy (
                argumentsInfo.Args
                |> List.fold (fun env arg ->
                    Map.add arg.Name arg.Type env) context.NameTypeEnv.Value
            )

        let stackAllocator = StackAllocator ()
        let funcBodyStr = StringBuilder 8096
        let endOfFunctionLabel = stackAllocator.AllocateLabel "function_global_end"
        let nasmContext =
            { NasmContext.CurrentSource = context.CurrentSource
              CurrentFunction = func
              Program = program
              StackEnv = stackEnv
              NameTypeEnv = nameTypeEnv
              Allocator = stackAllocator
              StringBuilder = funcBodyStr }

        do! checkWithContext nasmContext (writeStatements func.Body endOfFunctionLabel)


        fprintf $";;; %s{func.Name} ("
        let argsStr = func.Args |> List.map (fun (name, typ) -> sprintf $"%s{name} : %O{typ}") |> String.concat ", "
        fprintfn $"%s{argsStr}) : %O{func.ReturnType}"

        let funcLabel = getLabel' func.Name context.CurrentSource
        fprintfn "%%push"
        fprintfn $"static %s{funcLabel}"
        fprintfn $"%s{funcLabel}:"
        fprintfn $"enter %d{stackAllocator.TotalBytesAllocated}, 0"
        fprintfn ";;; Body"

        if not (List.isEmpty stackAllocator.Defers) then
            fprintfn $"; Clear all defers (%d{List.length stackAllocator.Defers})"
            fprintfn $"mov rax, %s{nasmFalseConst}"
            for deferBody in stackAllocator.Defers do
                fprintfn $"mov qword [rbp%+d{deferBody.Offset}], rax"
            fprintfn "; End of clear all defers"

        fprintf $"%O{funcBodyStr}"
        fprintfn ";;; End of body"
        fprintfn $" %s{endOfFunctionLabel}:"

        if not (List.isEmpty stackAllocator.Defers) then
            fprintfn $";;; Defers bodies go from here (%d{List.length stackAllocator.Defers})"
            for index, deferBody in List.rev (List.indexed stackAllocator.Defers) do
                let label = stackAllocator.AllocateLabel "defer_end"
                fprintfn $"; Defer #%d{index}"
                fprintfn $"mov rax, qword [rbp%+d{deferBody.Offset}]"
                fprintfn $"cmp rax, %s{nasmFalseConst}"
                fprintfn $"je %s{label}"
                fprintf $"%O{deferBody.Body}"
                fprintfn $"%s{label}:"
                fprintfn $"; End of defer #%d{index}"


        let argsSizesAligned = argumentsInfo.Args |> List.sumBy (fun s -> align s.Size 8)
        match TypeId.isVoid func.ReturnType with
        | true ->
            fprintfn "leave"
            fprintfn "pop rax ; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "push rax"
            fprintfn "ret"
        | false when argumentsInfo.IsResultSizeFitsReg ->
            fprintfn "pop rax ; Move result expression from stack to reg"
            fprintfn "leave"
            fprintfn "pop rbx ; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "push rbx"
            fprintfn "ret"
        | false ->
            fprintfn ";;; Copy result expression from stack to pointer from implicit first argument"
            fprintfn $"push %d{argumentsInfo.ResultSize}"
            fprintfn "mov rax, qword [rbp+16]"
            fprintfn "push rax"
            fprintfn "push rsp"
            fprintfn "memcopy"
            fprintfn $"add rsp, %d{align argumentsInfo.ResultSize 8}"
            fprintfn "leave"
            fprintfn "pop rax; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "add rsp, 8; Remove first implicit pointer argument"
            fprintfn "push rax"
            fprintfn "ret"

        fprintfn "%%pop\n\n"
    }

    let writeExportWrapperMicrosoftX64 (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! source = getFromContext (fun c -> c.CurrentSource)
        let! argsSizes =
            func.Args
            |> List.map snd
            |> List.map (fun t -> getTypeIdSize { TypeId = t; Source = source })
            |> unwrapList

        let copyRegArg reg (offset: int) (argSize: int) = (fun () ->
            match argSize with
            | 0 -> failwith "Can't copy argument of size 0."
            | 1 ->
                fprintfn $"mov rax, %s{reg}"
                fprintfn "and rax, 0ffh"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 2 ->
                fprintfn $"mov rax, %s{reg}"
                fprintfn "and rax, 0ffffh"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 4 ->
                fprintfn $"mov rax, %s{reg}"
                fprintfn "and rax, 0ffffffffh"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 8 ->
                fprintfn $"mov qword [rsp%+d{offset}, %s{reg}"
            | size ->
                fprintfn $"push %d{size}"
                fprintfn $"lea rax, qword [rsp%+d{offset}]"
                fprintfn "push rax"
                fprintfn $"push %s{reg}"
                fprintfn "memcopy")

        let copyFloatArg reg (offset: int) = (fun () ->
            fprintfn $"movsd qword [rsp%+d{offset}], %s{reg}")

        let copyStackArg index (offset: int) (argSize: int) = (fun () ->
            match int argSize with
            | 0 -> failwith "Can't copy argument of size 0"
            | 1 ->
                fprintfn "mov rax, 0"
                fprintfn $"mov rax, byte [rbp+%d{index * 8}]"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 2 ->
                fprintfn "mov rax, 0"
                fprintfn $"mov rax, word [rbp+%d{index * 8}]"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 4 ->
                fprintfn "mov rax, 0"
                fprintfn $"mov rax, dword [rbp+%d{index * 8}]"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | 8 ->
                fprintfn "mov rax"
                fprintfn $"mov rax, qword [rbp+%d{index * 8}]"
                fprintfn $"mov qword [rsp%+d{offset}], rax"
            | size ->
                fprintfn $"push %d{size}"
                fprintfn $"lea rax, qword [rsp%+d{offset}]"
                fprintfn "push rax"
                fprintfn $"mov rax, qword [rbp+%d{index * 8}]"
                fprintfn "push rax"
                fprintfn "memcopy")

        let copyArgsFromMsX64ToStack (offset, writer: unit -> unit) (argIndex, argType, argSize: int) =
            let newWriter =
                match argIndex with
                | 0 when TypeId.isFloat argType -> copyFloatArg "xmm0" offset
                | 0 -> copyRegArg "rcx" offset argSize
                | 1 when TypeId.isFloat argType -> copyFloatArg "xmm1" offset
                | 1 -> copyRegArg "rdx" offset argSize
                | 2 when TypeId.isFloat argType -> copyFloatArg "xmm2" offset
                | 2 -> copyRegArg "r8" offset argSize
                | 3 when TypeId.isFloat argType -> copyFloatArg "xmm3" offset
                | 3 -> copyRegArg "r9" offset argSize
                | index -> copyStackArg (index - 4) offset argSize
            let offset = offset + (align argSize 8)
            (offset, writer >> newWriter)

        let! funcReturnSize = getTypeIdSize { TypeId = func.ReturnType; Source = source }
        let returnSizeFitsReg = List.contains funcReturnSize [ 0; 1; 2; 4; 8 ]
        let firstArgIsResultPtr = not returnSizeFitsReg
        let dedicatedArgumentForNativeResultRequired = not (TypeId.isVoid func.ReturnType) && funcReturnSize > 8

        let arguments =
            List.indexed (List.zip (func.Args |> List.map snd) argsSizes)
            |> List.map (fun (index, (argType, argSize)) -> (index, argType, argSize))
        let arguments = if firstArgIsResultPtr then
                            arguments |> List.map (fun (index, argType, argSize) -> (index + 1, argType, argSize))
                        else arguments

        let (memoryToAllocate: int), argsToStackCopy =
            arguments |> List.fold copyArgsFromMsX64ToStack (0, (fun () -> ()))
        let memoryToAllocate = memoryToAllocate + 8 + (align funcReturnSize 8)

        fprintfn $";;; Export wrapper for '%s{func.Name}' function"
        fprintfn "%%push"
        fprintfn $"global %s{func.Name}"
        fprintfn $"%s{func.Name}:"
        fprintfn $"enter %d{memoryToAllocate}, 0"

        if firstArgIsResultPtr then
            fprintfn "mov qword [rbp-8], rcx"

        argsToStackCopy ()

        if dedicatedArgumentForNativeResultRequired then
            fprintfn $"lea rax, qword [rbp-%d{(align funcReturnSize 8) + 8}]"
            fprintfn "push rax"

        fprintfn $"call %s{getLabel' func.Name source}"

        match TypeId.isVoid func.ReturnType with
        | true ->
            fprintfn "mov rax, 0"
            fprintfn "leave"
            fprintfn "ret"
        | false when 1 <= funcReturnSize && funcReturnSize <= 8 ->
            match int funcReturnSize with
            | 0 -> failwith "Return size can't be 0 here."
            | _ when TypeId.isFloat func.ReturnType ->
                fprintfn "push rax"
                fprintfn "movsd xmm0, qword [rsp]"
                fprintfn "add rsp, 8"
                fprintfn "leave"
                fprintfn "ret"
            | 1 | 2 | 4 | 8 ->
                fprintfn "leave"
                fprintfn "ret"
            | _ ->
                fprintfn "mov rbx, qword [rbp-8]"
                fprintfn "mov qword [rbx], rax"
                fprintfn "mov rax, 0"
                fprintfn "leave"
                fprintfn "ret"
        | false ->
            fprintfn $"push %d{funcReturnSize}"
            fprintfn "mov rax, qword [rbp-8]"
            fprintfn "push rax"
            fprintfn $"lea rax, qword[rbp-%d{(align funcReturnSize 8) + 8}]"
            fprintfn "push rax"
            fprintfn "memcopy"
            fprintfn "mov rax, 0"
            fprintfn "leave"
            fprintfn "ret"
        fprintfn "%%pop"
        fprintfn $";;; End of export wrapper for '%s{func.Name}' function\n\n"
    }

    let writeExternWrapperMicrosoftX64 (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! source = getFromContext (fun c -> c.CurrentSource)
        let! argumentsInfo = getFuncCallInfo func
        let rcxOffset = -8
        let rdxOffset = -16
        let r8Offset = -24
        let r9Offset = -32
        let returnPtrOffset = (-(align argumentsInfo.ResultSize 8)) - 32

        let copyArgumentToRegister (regOffset: int) (arg: IncomingArgInfo) =
            match arg.Size with
            | 0 -> failwithf $"Argument can't have size 0: %A{arg}"
            | 1 | 2 | 4 | 8 -> (fun () ->
                fprintfn $"mov rax, qword [rbp%+d{arg.Offset}]"
                fprintfn $"mov qword [rbp%+d{regOffset}], rax")
            | _ -> (fun () ->
                    fprintfn $"lea, rax qword [rbp%+d{arg.Offset}]"
                    fprintfn $"mov qword [rbp%+d{regOffset}], rax")

        let copyArgumentToStack (stackOffset: int) (arg:IncomingArgInfo) =
            match int arg.Size with
            | 0 -> failwithf $"Argument can't have size 0: %A{arg}"
            | 1 | 2 | 4 | 8 -> (stackOffset + (align arg.Size 8), (fun () ->
                fprintfn $"mov rax, qword [rbp%+d{arg.Offset}]"
                fprintfn $"mov qword [rsp%+d{stackOffset}], rax"))
            | _ -> (stackOffset + 8, (fun () ->
                fprintfn $"lea rax, qword [rbp%+d{arg.Offset}]"
                fprintfn $"mov qword [rsp%+d{stackOffset}], rax"))

        let mapNativeArgumentIndexToExternIndex (stackOffset: int) (arg: IncomingArgInfo) =
                match arg.Index with
                | 0 when TypeId.isFloat arg.Type.TypeId -> (stackOffset, (fun () -> fprintfn $"movsd xmm0, qword [rbp%+d{arg.Offset}]"))
                | 0 -> (stackOffset, copyArgumentToRegister rcxOffset arg)
                | 1 when TypeId.isFloat arg.Type.TypeId -> (stackOffset, (fun () -> fprintfn $"movsd xmm1, qword [rbp%+d{arg.Offset}]"))
                | 1 -> (stackOffset, copyArgumentToRegister rdxOffset arg)
                | 2 when TypeId.isFloat arg.Type.TypeId -> (stackOffset, (fun () -> fprintfn $"movsd xmm2, qword [rbp%+d{arg.Offset}]"))
                | 2 -> (stackOffset, copyArgumentToRegister r8Offset arg)
                | 3 when TypeId.isFloat arg.Type.TypeId -> (stackOffset, (fun () -> fprintfn $"movsd xmm3, qword [rbp%+d{arg.Offset}]"))
                | 3 -> (stackOffset, copyArgumentToRegister r9Offset arg)
                | _ -> copyArgumentToStack stackOffset arg

        let isImplicitReturnPtrRequired = not (TypeId.isVoid func.ReturnType) && not argumentsInfo.IsResultSizeFitsReg
        let arguments = if isImplicitReturnPtrRequired
                        then argumentsInfo.Args |> List.map (fun a -> { a with Index = a.Index + 1 })
                        else argumentsInfo.Args

        let offset, argumentWriter =
            arguments
            |> List.fold (fun (offset, writer) arg ->
                let newOffset, newWriter = mapNativeArgumentIndexToExternIndex offset arg
                (newOffset, writer >> newWriter)) (0, (fun () -> ()))
        let memoryToAllocate = offset + 32 + (align argumentsInfo.ResultSize 8)

        let label = getLabel' func.Name source
        fprintfn $";;; Extern wrapper for func '%s{func.Name}'"
        fprintfn "%%push"
        fprintfn $"extern %s{func.Name}"
        fprintfn $"static %s{label}"
        fprintfn $"%s{label}:"
        fprintfn $"enter %d{memoryToAllocate}, 0"
        fprintfn "mov rax, 0"
        fprintfn $"mov qword [rbp%+d{rcxOffset}], rax"
        fprintfn $"mov qword [rbp%+d{rcxOffset}], rax"
        fprintfn $"mov qword [rbp%+d{r8Offset}], rax"
        fprintfn $"mov qword [rbp%+d{r9Offset}], rax"
        argumentWriter ()

        if isImplicitReturnPtrRequired then
            fprintfn $"lea rax, qword [rbp%+d{returnPtrOffset}]"
            fprintfn $"mov qword [rbp%+d{rcxOffset}], rax"

        fprintfn $"mov rcx, qword [rbp%+d{rcxOffset}]"
        fprintfn $"mov rdx, qword [rbp%+d{rdxOffset}]"
        fprintfn $"mov r8, qword [rbp%+d{r8Offset}]"
        fprintfn $"mov r9, qword [rbp%+d{r9Offset}]"
        fprintfn "sub rsp, 32"
        fprintfn $"call %s{func.Name}"
        fprintfn "add rsp, 32"

        let argumentsSum = arguments |> List.sumBy (fun s -> align s.Size 8)
        let argumentsSum = argumentsSum + if argumentsInfo.IsFirstArgumentResultPtr then 8 else 0

        match int argumentsInfo.ResultSize with
        | 0 ->
            fprintfn "leave"
            fprintfn "pop rax"
            fprintfn $"add rsp, %d{argumentsSum}"
            fprintfn "push rax"
            fprintfn "ret"
        | _ when TypeId.isFloat func.ReturnType ->
            fprintfn "sub rsp, 8"
            fprintfn "movsd qword [rsp], xmm0"
            fprintfn "mov rax, qword [rsp]"
            fprintfn "add rsp, 8"
            fprintfn "leave"
            fprintfn "pop rbx"
            fprintfn $"add rsp, %d{argumentsSum}"
            fprintfn "push rbx"
            fprintfn "ret"
        | 1 | 2 | 4 | 8 ->
            fprintfn "leave"
            fprintfn "pop rbx"
            fprintfn $"add rsp, %d{argumentsSum}"
            fprintfn "push rbx"
            fprintfn "ret"
        | _ when argumentsInfo.IsResultSizeFitsReg ->
            fprintfn $"mov rax, qword [rbp%+d{returnPtrOffset}]"
            fprintfn "leave"
            fprintfn "pop rbx"
            fprintfn $"add rsp, %d{argumentsSum}"
            fprintfn "push rbx"
            fprintfn "ret"
        | size ->
            fprintfn $"push %d{size}"
            fprintfn "mov rax, qword [rbp+16]"
            fprintfn "push rax"
            fprintfn $"lea rax, qword [rbp%+d{returnPtrOffset}]"
            fprintfn "push rax"
            fprintfn "memcopy"
            fprintfn "leave"
            fprintfn "pop rax"
            fprintfn $"add rsp, %d{argumentsSum}"
            fprintfn "push rax"
            fprintfn "ret"
        fprintfn "%%pop"
        fprintfn $";;; End of extern wrapper for func '%s{func.Name}'\n\n"
    }

    let writeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        match func.Modifier with
        | None -> do! writeNativeFunction func
        | Some Modifier.Export ->
            do! writeNativeFunction func
            match callconv with
            | CallingConvention.MicrosoftX64 ->
                do! writeExportWrapperMicrosoftX64 func
            | CallingConvention.SysVX64 -> failwith "Calling convention 'System V ABI x64' to be implemented'"
        | Some Modifier.Extern ->
            match callconv with
            | CallingConvention.MicrosoftX64 ->
                do! writeExternWrapperMicrosoftX64 func
            | CallingConvention.SysVX64 -> failwith "Calling convention 'System V ABI x64' to be implemented."
    }

    let rec writeVariableExpression (expression: Expression) (size: int) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let writePrefix () =
            match int size with
            | 1 -> fprintf "db "
            | 2 -> fprintf "dw "
            | 4 -> fprintf "dd "
            | 8 -> fprintf "dq "
            | size -> failwithf $"Size %d{size} should not happen."

        match expression with
        | Expression.Constant (Value.String string) ->
            let strLabel = getStringLabel string
            fprintfn $"dq %s{strLabel}"

        | Expression.Constant (Value.Number number) ->
            writePrefix ()
            fprintfn $"%s{number2nasm number}"

        | Expression.Constant (Value.Boolean value) ->
            match value with
            | true -> fprintfn $"dq %s{nasmTrueConst}"
            | false -> fprintfn $"dq %s{nasmFalseConst}"

        | Expression.Constant (Value.Char char) ->
            writePrefix ()
            fprintfn $"0%x{int char}h"

        | Expression.StructCreation (typename, fields) when fields |> List.map snd |> List.forall isExpressionConstant ->
            let! typDecl = locateTypeDecl typename
            fprintfn $";;; %A{typDecl.TypeDecl.TypeType} named '%O{typDecl}' goes from here"
            match typDecl.TypeDecl.TypeType with
            | TypeType.Struct ->
                for fieldName, fieldType in typDecl.TypeDecl.Fields do
                    let! size = getTypeIdSize { TypeId = fieldType; Source = typDecl.Source }
                    fprintfn $"; Field '%s{fieldName}'"
                    match MList.tryLookup fieldName fields with
                    | Some initExpression ->
                        do! writeVariableExpression initExpression size
                        fprintfn $"align %d{align size 8},db 0"
                    | None ->
                        fprintfn $"times %d{align size 8} db 0"

            | TypeType.Union ->
                let! source = getFromContext (fun c -> c.CurrentSource)
                let! size = getTypeIdSize { TypeId = TypeId.Named typename; Source = source }
                match fields with
                | [] -> fprintfn $"times %d{align size 8} db 0"
                | [ (fieldName, fieldExpr) ] ->
                    match MList.tryLookup fieldName typDecl.TypeDecl.Fields with
                    | Some fieldType ->
                        let! size = getTypeIdSize { TypeId = fieldType; Source = typDecl.Source }
                        do! writeVariableExpression fieldExpr size
                        fprintfn $"align %d{align size 8},db 0"
                    | None -> failwithf $"Can't find field '%s{fieldName}' in '%O{typDecl}'"
                | _ -> yield! fatalDiag $"Union initializer typ '%O{typDecl}' must consist of exactly 1 field initializer."

            fprintfn $";;; End of %A{typDecl.TypeDecl.TypeType} named '%O{typDecl}' goes from here"

        | expression ->
            let unionCase, _ = FSharpValue.GetUnionFields (expression, expression.GetType ())
            yield! fatalDiag $"Expression '%s{unionCase.Name}' can't be used for global variables intializers."
    }

    let writeVariable (var: Variable) aligner res: TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! source = getFromContext (fun c -> c.CurrentSource)
        let label = getLabel var.Name source var.Modifier
        let! size = getTypeIdSize { TypeId = var.TypeId; Source = source }

        match var.Modifier with
        | None -> fprintf "static "
        | Some Modifier.Extern -> fprintf "extern "
        | Some Modifier.Export -> fprintf "global "
        fprintfn $"%s{label}"
        fprintfn $"%s{label}:"
        match var.InitExpr with
        | Some expression -> do! writeVariableExpression expression size
        | None -> fprintfn $"%s{res (align size 8)}"

        fprintfn $"%s{aligner 16}"
    }

    let writeGlobalVariables () : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! source = getFromContext (fun c -> c.CurrentSource)
        let variables = source |> getVariableDeclarations
        let readonlyVars = variables |> List.filter (fun v -> TypeId.isConst v.TypeId)
        let writableVars = variables |> List.filter (fun v -> (not (TypeId.isConst v.TypeId)) && Option.isSome v.InitExpr)
        let bssVars = variables |> List.filter (fun v -> (not (TypeId.isConst v.TypeId)) && Option.isNone v.InitExpr)

        assert ((List.length readonlyVars + List.length writableVars + List.length bssVars) = List.length variables)

        if not (List.isEmpty variables) then
            fprintfn $";;; Variables (%d{List.length variables})\n"

        if not (List.isEmpty readonlyVars) then
            fprintfn $";;; Read only variables (%d{List.length readonlyVars})"
            fprintfn "section .rodata align=16"
            for variable in readonlyVars do
                do! writeVariable variable (fun b -> sprintf $"align %d{b},db 0") (fun b -> sprintf $"times %d{b} db 0")
                fprintf "\n"

        if not (List.isEmpty writableVars) then
            fprintfn $";;; Writable variables (%d{List.length writableVars})"
            fprintfn "section .data align=16"
            for variable in writableVars do
                do! writeVariable variable (fun b -> sprintf $"align %d{b},db 0") (fun b -> sprintf $"times %d{b} db 0")
                fprintf "\n"

        if not (List.isEmpty bssVars) then
            fprintfn $";;; bss (Unitialized) varaibles (%d{List.length bssVars})"
            fprintfn "section .bss align=16"
            for variable in bssVars do
                do! writeVariable variable (fun b -> sprintf $"align %d{b},resb 0") (fun b -> sprintf $"times %d{b} resb 0")
                fprintf "\n"
    }

    let writeMacros () =
        fprintfn "%%macro memcopy 0"
        fprintfn "    mov rsi, 0"
        fprintfn "    mov rdi, 0"
        fprintfn "    mov rcx, 0"
        fprintfn "    pop rsi"
        fprintfn "    pop rdi"
        fprintfn "    pop rcx"
        fprintfn "    rep movsb"
        fprintfn "%%endmacro"
        fprintf "\n"

    let runm (m: TypeCheckerM.M<SourceContext, unit>) (ctx: SourceContext) =
        match m ctx with
        | Ok ((), []) -> ()
        | Error [] -> ()
        | Ok ((), xs)
        | Error xs ->
            failwithf $"There are some errors running type checker context:\n%s{diags2Str xs}"

    interface ICodegenerator with

        member _.Write() =
            fprintfn "bits 64\n"
            writeMacros ()

            for source in program.Sources do
                fprintfn $";;; Source '%s{source.Filename}'\n"

                match makeContext source program with
                | Ok context ->
                    let functions = context.CurrentSource |> getFunctionDeclarations
                    if not (List.isEmpty functions) then
                        fprintfn $";;; Functions (%d{List.length functions})"
                        fprintfn "section .text align=16"
                        for func in functions do
                            runm (writeFunction func) context

                    runm (writeGlobalVariables ()) context
                | Error diags ->
                    failwithf $"Error creating type checker context:\n%s{diags2Str diags}"

                fprintfn $";;; End of source '%s{source.Filename}'\n"

            let strings =
                program.Sources
                |> Seq.ofList
                |> Seq.collect getStringsFromSource
                |> Seq.map (fun str -> (getStringLabel str, str))
                |> Seq.distinctBy fst
                |> Seq.sortBy (fun (_, str) -> String.length str)
                |> List.ofSeq

            if not (List.isEmpty strings) then
                fprintfn $";;; Now all strings gathered from sources (%d{List.length strings}) go here"
                fprintfn "section .rodata align=16\n"
                for label, str in strings do
                    fprintfn $"static %s{label}"
                    fprintfn $"%s{label}:"
                    fprintfn $"db %s{string2nasm str}"
                    fprintfn "align 16,db 0\n\n"
