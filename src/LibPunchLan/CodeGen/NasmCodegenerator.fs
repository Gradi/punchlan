module LibPunchLan.CodeGen.NasmCodegenerator

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

type StackAllocator () =
    let mutable totalBytesAllocated = 0<albytesize>
    let mutable labelCount = 0

    member _.TotalBytesAllocated = totalBytesAllocated

    member _.AllocateVar (name: string) (size: int<albytesize>) (env: Map<string, int<albytesize>>) =
        totalBytesAllocated <- totalBytesAllocated + size
        assert (((int size) % 8) = 0)
        assert (((int totalBytesAllocated) % 8) = 0)
        Map.add name -totalBytesAllocated env

    member _.AllocateAnonymousVar (size: int<albytesize>) =
            totalBytesAllocated <- totalBytesAllocated + size
            -totalBytesAllocated

    member _.AllocateLabel suffix =
        let label = sprintf $"%%$lbl%d{labelCount}_%s{suffix}"
        labelCount <- labelCount + 1
        label

type NasmContext =
    { CurrentSource: Source
      CurrentFunction: Function
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeRef>>
      StackEnv: Map<string, int<albytesize>>
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

type FieldInfo =
    { TypeDecl: TypeDeclRef
      Type: TypeRef
      Name: string
      Offset: int
      Size: int<bytesize> }

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
        sprintf $"(%f{dbl})"
    | Number.Negative _ -> failwith "Negative number should have been removed at this point."

let getField (typ: TypeDeclRef) (name: string) : TypeCheckerM.M<NasmContext, FieldInfo> = tchecker {
    let folder prev (fieldName, fieldType) : unit -> TypeCheckerM.M<NasmContext, int * FieldInfo list> = (fun () -> tchecker {
        let! offset, xs = prev ()
        let! size = checkWithContext' (fun c -> c.WithSource ()) (getTypeIdSize { TypeId = fieldType; Source = typ.Source })
        let xs = xs @ [ { FieldInfo.TypeDecl = typ; Type = { TypeId = fieldType; Source = typ.Source }; Name = fieldName; Offset = offset; Size = size } ]
        yield (offset + (int (align size 8)), xs)
    })

    let fields =
        typ.TypeDecl.Fields
        |> List.fold folder (fun () ->  tchecker { yield (0, []) })
    let! _, fields = fields ()

    match List.filter (fun f -> f.Name = name) fields with
    | [] -> yield! checkWithContext' (fun c -> c.WithFunction ()) (fatalDiag' $"Can't find field named '%s{name}' in '%O{typ}'.")
    | [ field ] -> yield field
    | fields -> yield! checkWithContext' (fun c -> c.WithFunction ()) (fatalDiag' $"Found more than 1 (%d{List.length fields}) fields named '%s{name}' in '%O{typ}'.")
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
        do! bprintfn "movxz rax, dword [rax]"
        do! bprintfn "push rax"
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
            do! bprintfn $"lea rax, byte [%s{stringLabel}]"
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
            let! funcReturnSize = sourceContext (getTypeIdSize { TypeId = func.Function.ReturnType; Source = func.Source})
            let funcReturnSizeFitsReg = 1<bytesize> <= funcReturnSize && funcReturnSize <= 8<bytesize>
            let! allocator = getFromContext (fun c -> c.Allocator)
            let resultPointerRequired = not (TypeId.isVoid func.Function.ReturnType) && not funcReturnSizeFitsReg

            match func.Function.Modifier with
            | None
            | Some Modifier.Export ->
                for expression in List.rev args do
                    do! writeExpression expression

                let returnValuePtr = if resultPointerRequired
                                     then allocator.AllocateAnonymousVar (align funcReturnSize 8)
                                     else 0<albytesize>

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
                    do! bprintfn $"lea rax, qword [rbp-%d{returnValuePtr}]"
                    do! bprintfn "push rax"
                    do! bprintfn "memcopy"

            | Some Modifier.Extern ->
                failwith "Extern functions to be implemented."

        | Expression.MemberAccess (Expression.Variable variableName as leftExpr, memberName)
            when isStructVariableWithMember variableName memberName ssourceContext ->
            let! typ = sourceContext (getExpressionType leftExpr)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldInfo = getField typeDecl memberName
                do! writeExpressionAddress leftExpr
                do! bprintfn "pop rax"
                do! bprintfn $"add rax, %d{fieldInfo.Offset}"
                do! bprintfn "push rax"
                do! writeCopyToStack fieldInfo.Type
            | typ -> failwithf $"'%O{typ}' should have been already covered."

        | Expression.MemberAccess (Expression.Variable alias, name) when getAliasedSource alias ssourceContext |> Result.isOk ->
            let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = Some alias })
            let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
            do! bprintfn $"lea rax, qword [%s{label}]"
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
            do! bprintfn "pop rbx"
            do! bprintfn "pop rax"
            do! bprintfn $"mul rbx, rbx, %+d{size}"
            do! bprintfn "lea rax, qword [rax+rbx]"
            do! bprintfn "push rax"
            do! writeCopyToStack { TypeId = arraySubitemType; Source = arrayType.Source }

        | Expression.StructCreation (name, fields) as expr ->
            failwith "Struct creation to be implemented."

        | Expression.Bininversion expr ->
            do! writeExpression expr
            do! bprintfn "pop rax"
            do! bprintfn "not rax"
            do! bprintfn "push rax"
    }

    and writeBinaryExpression (expression: BinaryExpression) : TypeCheckerM.M<NasmContext, unit> = tchecker {
        let left = expression.Left
        let right = expression.Right
        let! leftType = sourceContext (getExpressionType left)
        let! rightType = sourceContext (getExpressionType left)
        let leftType = leftType.TypeId
        let rightType = rightType.TypeId
        let! allocator = getFromContext (fun c -> c.Allocator)

        match expression.Kind with
        | BinaryExpressionKind.Plus ->

            do! writeExpression right
            do! writeExpression left

            if (TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if (TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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
                do! bprintfn "mul rax, rbx"
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
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
                do! bprintfn $"lea rax, byte [rbp%+d{stackOffset}]"
                do! bprintfn "push rax"
            | None ->
                let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = None })
                let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
                do! bprintfn $"lea rax, byte [%s{label}]"
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
            do! bprintfn $"lea rax, byte [%s{label}]"
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

            do! writeExpressionAddress array
            do! writeExpression index
            do! bprintfn "pop rbx"
            do! bprintfn "pop rax"
            do! bprintfn $"mul rbx, rbx, %d{size}"
            do! bprintfn "lea rax, byte [rax+rbx]"
            do! bprintfn "push rax"

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
            let _, writerThatWritesAllIfs =
                elseIfs |> List.fold folder (elseBranch, (fun () -> tchecker { yield () }))

            do! writeIfCond mainCond elseBranch
            do! writerThatWritesAllIfs ()
            do! bprintfn $"%s{endOfIfBranch}:"

            yield! context

        | Statement.For (indexVariable, startExpression, endExpression, stepExpression, body) ->
            let! source, allocator, stackEnv, nameTypeEnv = getFromContext (fun c -> (c.CurrentSource, c.Allocator, c.StackEnv, c.NameTypeEnv))
            let newStackEnv = allocator.AllocateVar indexVariable 8<albytesize> stackEnv
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

        | Statement.Defer _ ->
            yield! sourceFuncContext (fatalDiag' "Defer statement support not implemented.")

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
            | false when 1<bytesize> <= size && size <= 8<bytesize> ->
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
        let! argsSizes =
            func.Args |>
            List.map snd |>
            List.map (fun t -> getTypeIdSize { TypeId = t; Source = context.CurrentSource })
            |> unwrapList
        let! funcReturnSize = getTypeIdSize { TypeId = func.ReturnType; Source = context.CurrentSource }
        let returnSizeFitsReg = 1<bytesize> <= funcReturnSize && funcReturnSize <= 8<bytesize>
        let firstArgIsResultPtr = not (TypeId.isVoid func.ReturnType) && not returnSizeFitsReg
        let initialArgOffset = if firstArgIsResultPtr then 24<albytesize> else 16<albytesize> // 24 is (old rbp(8) + ret addr(8) + pointer(8)), 16 is (old rbp(8) + ret addr(8))

        let stackEnv, _ =
            List.zip (List.map fst func.Args) argsSizes
            |> List.fold (fun (env, offset) (name, size) ->
                let env = Map.add name offset env
                let offset = offset + (align size 8)
                (env, offset)) (Map.empty, initialArgOffset)

        let nameTypeEnv = lazy (
                func.Args
                |> List.fold (fun env (name, typeId) ->
                    Map.add name { TypeId = typeId; Source = context.CurrentSource } env) context.NameTypeEnv.Value
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
        fprintfn $"%O{funcBodyStr}"
        fprintfn ";;; End of body"
        fprintfn $" %s{endOfFunctionLabel}:"

        let argsSizesAligned = argsSizes |> List.sumBy (fun s -> align s 8)
        match TypeId.isVoid func.ReturnType with
        | true ->
            fprintfn "leave"
            fprintfn "pop rax ; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "push rax"
            fprintfn "ret"
        | false when returnSizeFitsReg ->
            fprintfn "pop rax ; Move result expression from stack to reg"
            fprintfn "leave"
            fprintfn "pop rbx ; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "push rbx"
            fprintfn "ret"
        | false ->
            fprintfn ";;; Copy result expression from stack to pointer from implicit first argument"
            fprintfn $"push %d{funcReturnSize}"
            fprintfn "mov rax, qword [rbp+16]"
            fprintfn "push rax"
            fprintfn "push rsp"
            fprintfn "memcopy"
            fprintfn $"add rsp, %d{align funcReturnSize 8}"
            fprintfn "leave"
            fprintfn "pop rax; Return address"
            fprintfn $"add rsp, %d{argsSizesAligned} ; Remove incoming arguments"
            fprintfn "add rsp, 8; Remove first implicit pointer argument"
            fprintfn "push rax"
            fprintfn "ret"

        fprintfn "%%pop"
    }

    let writeExportWrapperMicrosoftX64 (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! source = getFromContext (fun c -> c.CurrentSource)
        let! argsSizes = func.Args |> List.map snd |> List.map (fun t -> getTypeIdSize { TypeId = t; Source = source }) |> unwrapList

        let copyRegArg reg (offset: int<albytesize>) (argSize: int<bytesize>) = (fun () ->
            match int argSize with
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

        let copyFloatArg reg (offset: int<albytesize>) = (fun () ->
            fprintfn $"movsd qword [rsp%+d{offset}], %s{reg}")

        let copyStackArg index (offset: int<albytesize>) (argSize: int<bytesize>) = (fun () ->
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

        let copyArgsFromMsX64ToStack (offset, writer: unit -> unit) (argIndex, argType, argSize: int<bytesize>) =
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
        let returnSizeFitsReg = List.contains funcReturnSize [ 0<bytesize>; 1<bytesize>; 2<bytesize>; 4<bytesize>; 8<bytesize> ]
        let firstArgIsResultPtr = not returnSizeFitsReg
        let dedicatedArgumentForNativeResultRequired = not (TypeId.isVoid func.ReturnType) && funcReturnSize > 8<bytesize>

        let arguments =
            List.indexed (List.zip (func.Args |> List.map snd) argsSizes)
            |> List.map (fun (index, (argType, argSize)) -> (index, argType, argSize))
        let arguments = if firstArgIsResultPtr then
                            arguments |> List.map (fun (index, argType, argSize) -> (index + 1, argType, argSize))
                        else arguments

        let (memoryToAllocate: int<albytesize>), argsToStackCopy =
            arguments |> List.fold copyArgsFromMsX64ToStack (0<albytesize>, (fun () -> ()))
        let memoryToAllocate : int<albytesize> = if firstArgIsResultPtr then memoryToAllocate + 8<albytesize> else memoryToAllocate
        let memoryToAllocate : int<albytesize> = if not (1<bytesize> <= funcReturnSize && funcReturnSize <= 8<bytesize>) then memoryToAllocate + (align funcReturnSize 8) else memoryToAllocate

        fprintfn $";;; Export wrapper for '%s{func.Name}' function"
        fprintfn "%%push"
        fprintfn $"global %s{func.Name}"
        fprintfn $"%s{func.Name}:"
        fprintfn $"enter %d{memoryToAllocate}, 0"

        if firstArgIsResultPtr then
            fprintfn "mov qword [rbp-8], rcx"

        argsToStackCopy ()

        if dedicatedArgumentForNativeResultRequired then
            fprintfn $"lea rax, qword [rbp-%d{(align funcReturnSize 8) + 8<albytesize>}]"
            fprintfn "push rax"

        fprintfn $"call %s{getLabel' func.Name source}"

        match TypeId.isVoid func.ReturnType with
        | true ->
            fprintfn "mov rax, 0"
            fprintfn "leave"
            fprintfn "ret"
        | false when 1<bytesize> <= funcReturnSize && funcReturnSize <= 8<bytesize> ->
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
            fprintfn $"lea rax, qword[rbp-%d{(align funcReturnSize 8) + 8<albytesize>}]"
            fprintfn "push rax"
            fprintfn "memcopy"
            fprintfn "mov rax, 0"
            fprintfn "leave"
            fprintfn "ret"
        fprintfn "%%pop"
        fprintfn $";;; End of export wrapper for '%s{func.Name}' function"
    }

    let writeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        match func.Modifier with
        | None -> do! writeNativeFunction func
        | Some Modifier.Export ->
            do! writeNativeFunction func
            match callconv with
            | CallingConvention.MicrosoftX64 -> do! writeExportWrapperMicrosoftX64 func
            | CallingConvention.SysVX64 -> failwith "Calling convention 'System V ABI x64' to be implemented'"
        | Some Modifier.Extern ->
            failwith "Extern functions to be implemented."
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

    let runm (m: TypeCheckerM.M<SourceContext, unit>) (ctx: SourceContext) =
        match m ctx with
        | Ok ((), []) -> ()
        | Error [] -> ()
        | Ok ((), xs)
        | Error xs ->
            failwithf $"There are some errors running type checker context:\n%s{diags2Str xs}"

    interface ICodegenerator with

        member _.Write() =
            fprintfn "bits 64"
            writeMacros ()

            for source in program.Sources do
                fprintfn $";;; Source '%s{source.Filename}'"
                fprintfn "section .text align=16"

                match makeContext source program with
                | Ok context ->
                    for func in context.CurrentSource |> getFunctionDeclarations do
                        runm (writeFunction func) context
                | Error diags ->
                    failwithf $"Error creating type checker context:\n%s{diags2Str diags}"

                fprintfn $";;; End of source '%s{source.Filename}'"
