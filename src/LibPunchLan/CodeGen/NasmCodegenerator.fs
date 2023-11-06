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
    let mutable totalBytesAllocated = 0
    let mutable labelCount = 0

    member _.TotalBytesAllocated = totalBytesAllocated

    member _.AllocateVar (name: string) (size: int) (env: Map<string, int>) =
        totalBytesAllocated <- totalBytesAllocated + size
        Map.add name -totalBytesAllocated env

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

let getFieldOffsetSizeType (typ: TypeDeclRef) (name: string) : TypeCheckerM.M<NasmContext, int * int * TypeRef> = tchecker {
    let folder prev (fieldName, fieldType) : unit -> TypeCheckerM.M<NasmContext, int * (string * (int * int * TypeRef)) list> = (fun () -> tchecker {
        let! offset, xs = prev ()
        let! size = checkWithContext' (fun c -> c.WithSource ()) (getTypeIdSize { TypeId = fieldType; Source = typ.Source })
        let size = align size 8
        let xs = xs @ [ (name, (offset, size, { TypeRef.TypeId = fieldType; Source = typ.Source })) ]
        yield (offset + size, xs)
    })

    let fields =
        typ.TypeDecl.Fields
        |> List.fold folder (fun () ->  tchecker { yield (0, []) })
    let! _, fields = fields ()

    match MList.tryLookup name fields with
    | Some fieldData -> yield fieldData
    | None -> yield! checkWithContext' (fun c -> c.WithFunction ()) (fatalDiag' $"Can't find member '%s{name}' in '%O{typ}'")
}


let writeCopyToStack (typ: TypeRef) : TypeCheckerM.M<NasmContext, unit> = tchecker {
    do! bprintfn """
mov rax, 0
pop rax
"""
    match TypeId.unwrapConst typ.TypeId with
    | TypeId.Uint8
    | TypeId.Char ->
        do! bprintfn """
movzx rax, byte [rax]
push rax
"""
    | TypeId.Uint16 ->
        do! bprintfn """
movzx rax, word [rax]
push rax
"""
    | TypeId.Uint32 ->
        do! bprintfn """
movxz rax, dword [rax]
push rax
"""
    | TypeId.Uint64 ->
        do! bprintfn """
mov rax, qword [rax]
push rax
"""
    | TypeId.Int8 ->
        do! bprintfn """
movsx rax, byte [rax]
push rax
"""
    | TypeId.Int16 ->
        do! bprintfn """
movsx rax, word [rax]
push rax
"""
    | TypeId.Int32 ->
        do! bprintfn """
movsx rax, dword [rax]
push rax
"""
    | TypeId.Int64
    | TypeId.Bool
    | TypeId.Pointer _ ->
        do! bprintfn """
mov rax, qword [rax]
push rax
"""
    | TypeId.Void -> yield! sourceFuncContext (fatalDiag' "Can't copy to stack void type.")
    | TypeId.Float ->
        do! bprintfn """
fld dword [rax]
sub rsp, 8
fstp qword [rsb]
"""
    | TypeId.Double ->
        do! bprintfn """
mov rax, qword [rax]
push rax
"""
    | TypeId.Const _  as typ -> yield! sourceFuncContext (fatalDiag' $"Const(%O{typ}) should have been removed at this point.")
    | TypeId.Named typename  ->
        let! size = sourceContext (getTypeIdSize typ)
        let! allocator = getFromContext (fun c -> c.Allocator)
        let beginLabel = allocator.AllocateLabel "named_type_copy_begin"
        let againLabel = allocator.AllocateLabel "named_type_copy_again"
        let endLabel = allocator.AllocateLabel "named_type_copy_end"
        do! bprintfn $"""
;;; Copying struct\union '%O{typ}' of %d{size} bytes size
sub rsp, %d{align size 8} ; Aligned %d{size} to 8 bytes
mov rbx, 0
jmp %s{beginLabel}
%s{againLabel}:
add rbx, 1
%s{beginLabel}:
cmp rbx, %d{size}
je %s{endLabel}
mov rcx, 0
mov rcx, byte [rax+rbx]
mov byte [rsp+rbx] rcx
jmp $s{againLabel}
%s{endLabel}:
;;; End of copying struct\union '%O{typ}' of %d{size} size
"""
}

let writeCopyFromStack (typ: TypeRef) : TypeCheckerM.M<NasmContext, unit> = tchecker {
    do! bprintfn """
mov rax, 0
pop rax
"""

    match TypeId.unwrapConst typ.TypeId with
    | TypeId.Char
    | TypeId.Int8
    | TypeId.Uint8 ->
        do! bprintfn """
mov rbx, 0
pop rbx
mov byte [rax], rbx
"""
    | TypeId.Int16
    | TypeId.Uint16 ->
        do! bprintfn """
mov rbx, 0
pop rbx
mov word [rax], rbx
"""
    | TypeId.Int32
    | TypeId.Uint32
    | TypeId.Float ->
        do! bprintfn """
mov rbx, 0
pop rbx
mov dword [rax], rbx
"""
    | TypeId.Int64
    | TypeId.Uint64
    | TypeId.Double
    | TypeId.Pointer _
    | TypeId.Bool ->
        do! bprintfn """
mov rbx, 0
pop rbx
mov qword [rax], rbx
"""
    | TypeId.Void -> yield! sourceFuncContext (fatalDiag' "Type 'void' cannot be copied from stack.")
    | TypeId.Named typename ->
        let! size = sourceContext (getTypeIdSize typ)
        let! endOfCopy = getFromContext (fun c -> c.Allocator.AllocateLabel "copy_end")
        let! again = getFromContext (fun c -> c.Allocator.AllocateLabel "copy_next_byte")

        do! bprintfn $"""
;;; Copying '%O{typ}' from stack
mov rbx, 0
%s{again}:
cmp rbx, %d{size}
je %s{endOfCopy}
mov rcx, 0
mov rcx, byte [rsp+rbx]
mov byte [rax+rbx], rcx
inc rbx
jmp %s{again}
%s{endOfCopy}:
add rsp, %d{align size 8}
;;; End of copying '%O{typ}' from stack
"""
    | typ -> failwithf $"Type '%O{typ}' should have been covered"
}


type NasmCodegenerator (tw: TextWriter, program: Program) =

    let fprintf text = Printf.fprintf tw text
    let fprintfn text =
        Printf.fprintf tw text
        Printf.fprintf tw "\n"


    let rec writeExpression (expression: Expression) : TypeCheckerM.M<NasmContext, unit>  = tchecker {
        let! ssourceContext = getFromContext (fun c -> c.WithSource ())

        match expression with

        | Expression.Constant (Value.String str) ->
            let stringLabel = getStringLabel str
            do! bprintfn $"""
mov rax, 0
lea rax, byte [%s{stringLabel}]
push rax
"""

        | Expression.Constant (Value.Boolean bool) ->
            let boolStr =
                match bool with
                | true -> "ffffffffffffffffh"
                | false -> "00h"
            do! bprintfn $"""
mov rax, 0
mov rax, %s{boolStr}
push rax
            """

        | Expression.Constant (Value.Char char) ->
            do! bprintfn $"""
mov rax, 0
mov rax, %x{byte char}h
push rax
            """

        | Expression.Constant (Value.Number number) ->
            let number = number2nasm number
            do! bprintfn $"""
mov rax, 0
mov rax, %s{number}
push rax
"""

        | Expression.Variable name as expression ->
            let! exprType = sourceContext (getExpressionType expression)
            do! writeExpressionAddress expression
            do! writeCopyToStack exprType

        | Expression.FuncCall (name, args) ->
            let! func = sourceContext (locateFunctionDecl name)
            match func.Function.Modifier with
            | None
            | Some Modifier.Export ->
                for expression in List.rev args do
                    do! writeExpression expression
                let label = getLabel' name.Name func.Source
                do! bprintfn $"call %s{label}"
            | Some Modifier.Extern ->
                failwith "Extern functions to be implemented."

        | Expression.MemberAccess (Expression.Variable variableName as leftExpr, memberName)
            when isStructVariableWithMember variableName memberName ssourceContext ->
            let! typ = sourceContext (getExpressionType leftExpr)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldOffset, fieldSize, fieldType = getFieldOffsetSizeType typeDecl memberName
                do! writeExpressionAddress leftExpr
                do! bprintfn $"""
mov rax, 0
pop rax
add rax, %d{fieldOffset}
push rax
"""
            | typ -> failwithf $"'%O{typ}' should have been already covered."

        | Expression.MemberAccess (Expression.Variable alias, name) when getAliasedSource alias ssourceContext |> Result.isOk ->
            let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = Some alias })
            let! size = sourceContext (getTypeIdSize { TypeId = varDecl.Variable.TypeId; Source  = varDecl.Source })
            let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
            do! bprintfn $"""
mov rax, 0
lea rax, byte [%s{label}]
push rax
"""
            do! writeCopyToStack { TypeId = varDecl.Variable.TypeId; Source = varDecl.Source }

        | Expression.MemberAccess (left, name) ->
            let! typ = sourceContext (getExpressionType left)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldOffset, fieldSize, fieldType = getFieldOffsetSizeType typeDecl name
                do! writeExpressionAddress left
                do! bprintfn $"""
mov rax, 0
pop rax
add rax, %d{fieldOffset}
push rax
"""
                do! writeCopyToStack fieldType
            | typ -> yield! sourceFuncContext (fatalDiag' $"Member access to '%O{typ}'.%s{name} is not supported.")

        | Expression.BinaryExpression binExpr -> do! writeBinaryExpression binExpr

        | Expression.ArrayAccess (left, indexExpr) ->
            let! arrayType = sourceContext (getExpressionType left)
            let! arraySubitemType = sourceContext (getArraySubitemType arrayType.TypeId)
            let! size = sourceContext (getTypeIdSize { TypeId = arraySubitemType; Source = arrayType.Source })

            do! writeExpressionAddress left
            do! writeExpression indexExpr
            do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rbx
pop rax
mul rbx, rbx, %+d{size}
lea rax, [rax+rbx]
push rax
"""
            do! writeCopyToStack { TypeId = arraySubitemType; Source = arrayType.Source }

        | Expression.StructCreation (name, fields) as expr ->
            failwithf "Struct creation to be implemented."

        | Expression.Bininversion expr ->
            do! writeExpression expr
            do! bprintfn """
mov rax, 0
pop rax
not rax
push rax
"""
    }

    and writeBinaryExpression (expression: BinaryExpression) : TypeCheckerM.M<NasmContext, unit> = tchecker {
        match expression with
        | BinaryExpression.Plus (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            do! writeExpression right
            do! writeExpression left

            if (TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) then

                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
add rax, rbx
push rax
"""

            elif TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
add rax, rbx
push rax
"""

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn """
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
faddp
sub rsp, 8
fstp qword [rsp]
"""

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' + '%O{rightType}' should have been covered.")

        | BinaryExpression.Minus (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            do! writeExpression right
            do! writeExpression left

            if (TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) then

                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
rub rax, rbx
push rax
"""

            elif TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
sub rax, rbx
push rax
"""

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn """
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fsubp
sub rsp, 8
fstp qword [rsp]
"""

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' - '%O{rightType}' should have been covered.")

        | BinaryExpression.Multiply (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            do! writeExpression right
            do! writeExpression left

            if TypeId.isSigned leftType && TypeId.isSigned rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
imul rax, rbx
push rax
"""
            elif TypeId.isUnsigned leftType && TypeId.isUnsigned rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
mul rax, rbx
push rax
"""

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn """
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fmulp
sub rsp, 8
fstp qword [rsp]
"""

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType} * '%O{rightType}' should have been covered.")

        | BinaryExpression.Division (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            do! writeExpression right
            do! writeExpression left

            if TypeId.isSigned leftType && TypeId.isSigned rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
idiv rax, rbx
push rax
"""
            elif TypeId.isUnsigned leftType && TypeId.isUnsigned rightType then
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
div rax, rbx
push rax
"""
            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn """
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fdivp
sub rsp, 8
fstp qword [rsp]
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' / '%O{rightType}' should have been covered.")

        | BinaryExpression.Equal (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! allocator = getFromContext (fun c -> c.Allocator)
            let label1 = allocator.AllocateLabel "equal"
            let label2 = allocator.AllocateLabel "equal_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) ||
               (TypeId.isFloat leftType && TypeId.isFloat rightType)  ||
               (TypeId.unwrapConst leftType = TypeId.Bool && TypeId.unwrapConst rightType = TypeId.Bool) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
je %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' == '%O{rightType}' should have been covered.")

        | BinaryExpression.NotEqual (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! allocator = getFromContext (fun c -> c.Allocator)
            let label1 = allocator.AllocateLabel "not_equal"
            let label2 = allocator.AllocateLabel "not_equal_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) ||
               (TypeId.unwrapConst leftType = TypeId.Bool && TypeId.unwrapConst rightType = TypeId.Bool) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
jne %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' != '%O{rightType}' should have been covered.")

        | BinaryExpression.Less (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! allocator = getFromContext (fun c -> c.Allocator)
            let label1 = allocator.AllocateLabel "less"
            let label2 = allocator.AllocateLabel "less_end"

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
               (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
jb %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""

            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn $"""
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fcompp
mov rax, 0
fsctw al
cmp rax, 001b
je %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""

            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' < '%O{rightType}' should have been covered.")

        | BinaryExpression.LessOrEqual (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "less_or_equal")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "less_or_equal_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType)) ||
               (TypeId.isIntegerType leftType && TypeId.isIntegerType rightType) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
jbe %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn $"""
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fcompp
mov rax, 0
fsctw al
cmp rax, 001b
je %s{label1}
cmp rax, 100b
je %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' <= '%O{rightType}' should have been covered.")

        | BinaryExpression.Greater (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) ||
                (TypeId.isIntegerType leftType || TypeId.isIntegerType rightType)) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
cmp rax, rbx
ja %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
                """
            elif TypeId.isFloat leftType && TypeId.isFloat rightType then
                do! bprintfn $"""
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fcompp
mov rax, 0
fsctw al
cmp rax, 000b
je %s{label1}
push 00h
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' < '%O{rightType}' should have been covered.")

        | BinaryExpression.GreaterOrEqual (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext (getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! label1 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_or_equal")
            let! label2 = getFromContext (fun c -> c.Allocator.AllocateLabel "greater_or_equal_end")

            do! writeExpression right
            do! writeExpression left

            if ((TypeId.isPointerType leftType || TypeId.isPointerType rightType) &&
                (TypeId.isUnsigned leftType || TypeId.isUnsigned rightType) ||
                (TypeId.isIntegerType leftType || TypeId.isIntegerType rightType)) then

                do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
jae %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            elif TypeId.isFloat leftType && TypeId.isFloat rightType then

                do! bprintfn $"""
fld qword [rsp]
fld qword [rsp+8]
add rsp, 16
fcompp
mov rax, 0
fsctw al
cmp rax, 000b
jmp %s{label1}
cmp rax, 100b
jmp %s{label1}
push 00h
jmp %s{label2}
%s{label1}:
push ffffffffffffffffh
%s{label2}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' >= '%O{rightType}' should have been covered.")

        | BinaryExpression.Or (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext(getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! rightBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_right_branch")
            let! endOfOrBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_end_of_branch")
            let! falseBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "or_false_branch")

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
or rax, rbx
push rax
"""
            elif TypeId.unwrapConst leftType = TypeId.Bool && TypeId.unwrapConst rightType = TypeId.Bool then
                do! writeExpression left
                do! bprintfn $"""
mov rax, 0
pop rax
cmp rax, 0
je %s{rightBranch}
push ffffffffffffffffh
jmp %s{endOfOrBranch}
%s{rightBranch}:
"""
                do! writeExpression right
                do! bprintfn $"""
mov rax, 0
pop rax
cmp rax, 0
je %s{falseBranch}
push ffffffffffffffffh
jmp %s{endOfOrBranch}
%s{falseBranch}:
push 00h
%s{endOfOrBranch}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' or '%O{rightType}' should have been covered.")

        | BinaryExpression.And (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext(getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId
            let! falseBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "and_false_branch")
            let! endOfAndBranch = getFromContext (fun c -> c.Allocator.AllocateLabel "and_end_of_branch")

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
and rax, rbx
push rax
"""
            elif TypeId.unwrapConst leftType = TypeId.Bool && TypeId.unwrapConst rightType = TypeId.Bool then
                do! writeExpression left
                do! bprintfn $"""
mov rax, 0
pop rax
cmp rax, 0
je %s{falseBranch}
"""
                do! writeExpression right
                do! bprintfn $"""
mov rax, 0
pop rax
cmp rax, 0
je %s{falseBranch}
push ffffffffffffffffh
jmp %s{endOfAndBranch}
%s{falseBranch}:
push 00h
%s{endOfAndBranch}:
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' and '%O{rightType}' should have been covered")

        | BinaryExpression.Xor (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext(getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
xor rax, rbx
push rax
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' xor '%O{rightType}' should have been covered.")

        | BinaryExpression.RShift (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext(getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
shr rax, rbx
push rax
"""
            else yield! sourceFuncContext (fatalDiag' $"Operation '%O{leftType}' >> '%O{rightType}' should have been covered.")

        | BinaryExpression.LShift (left, right) ->
            let! leftType = sourceContext (getExpressionType left)
            let! rightType = sourceContext(getExpressionType right)
            let leftType = leftType.TypeId
            let rightType = rightType.TypeId

            if TypeId.isIntegerType leftType && TypeId.isIntegerType rightType then
                do! writeExpression right
                do! writeExpression left
                do! bprintfn """
mov rax, 0
mov rbx, 0
pop rax
pop rbx
shl rax, rbx
push rax
"""
    }

    and writeExpressionAddress (expression: Expression) : TypeCheckerM.M<NasmContext, unit> = tchecker {
        let! stackEnv = getFromContext (fun c -> c.StackEnv)
        let! ssourceContext = getFromContext (fun c -> c.WithSource ())

        match expression with
        | Expression.Variable name ->
            match Map.tryFind name stackEnv with
            | Some stackOffset ->
                do! bprintfn $"""
mov rax, 0
lea rax, byte [rbp%+d{stackOffset}]
push rax
"""
            | None ->
                let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = None })
                let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
                do! bprintfn $"""
mov rax, 0
lea rax, byte [%s{label}]
push rax
"""

        | Expression.MemberAccess (Expression.Variable varname, memberName)
            when isStructVariableWithMember varname memberName ssourceContext ->
            let! varDecl = sourceContext (locateVariableDecl { Name = varname; Alias = None })
            match TypeId.unwrapConst varDecl.Variable.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = varDecl.Source }) (locateTypeDecl typename)
                let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
                let! fieldOffset, fieldSize, _ = getFieldOffsetSizeType typeDecl memberName
                do! bprintfn $"""
mov rax, 0
lea rax, byte [%s{label}]
add rax, %d{fieldOffset}
push rax
"""
            | typ -> failwithf $"Should not happen. Type '%O{typ}' should have been covered by isStructVariableWithMember function."

        | Expression.MemberAccess (Expression.Variable alias, name)
            when getAliasedSource alias ssourceContext |> Result.isOk ->
            let! varDecl = sourceContext (locateVariableDecl { Name = name; Alias = Some alias })
            let label = getLabel varDecl.Variable.Name varDecl.Source varDecl.Variable.Modifier
            do! bprintfn $"""
mov rax, 0
lea rax, byte [%s{label}]
push rax
"""

        | Expression.MemberAccess (left, memberName) ->
            let! typ = sourceContext (getExpressionType left)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Named typename ->
                let! typeDecl = checkWithContext' (fun c -> { c.WithSource () with CurrentSource = typ.Source }) (locateTypeDecl typename)
                let! fieldOffset, fieldSize, fieldType = getFieldOffsetSizeType typeDecl memberName
                do! writeExpressionAddress left
                do! bprintfn $"""
mov rax, 0
pop rax
add rax, %d{fieldOffset}
push rax
"""
            | typ -> yield! sourceFuncContext (fatalDiag' $"Member access '%O{typ}'.%s{memberName} is not supported.")

        | Expression.ArrayAccess (array, index) ->
            let! arrayType = sourceContext (getExpressionType array)
            let! arraySubitemType = sourceContext (getArraySubitemType arrayType.TypeId)
            let! size = sourceContext (getTypeIdSize { TypeId = arraySubitemType; Source = arrayType.Source })

            do! writeExpressionAddress array
            do! writeExpression index
            do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rbx
pop rax
mul rbx, rbx, %d{size}
lea rax, byte [rax+rbx]
push rax
"""

        | expression ->
            let! typ = sourceContext (getExpressionType expression)
            match TypeId.unwrapConst typ.TypeId with
            | TypeId.Pointer _ ->
                do! writeExpression expression
            | typ ->
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
                do! bprintf $"""
mov rax, 0
pop rax
cmp rax, 0
je %s{elseBranch}
"""
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
            do! bprintfn $"""
mov rax, 0
mov rbx, 0
pop rax
pop rbx
cmp rax, rbx
je %s{endOfForLoopLabel}
"""
            do! checkWithContext newContext (writeStatements body endOfFunctionLabel)
            do! checkWithContext newContext (writeExpression stepExpression)
            do! checkWithContext newContext (writeExpressionAddress (Expression.Variable indexVariable))
            do! checkWithContext newContext (writeCopyFromStack { TypeId = TypeId.Int64; Source = source })
            do! bprintfn $"""
jmp %s{compareIndexVarLabel}
%s{endOfForLoopLabel}:
"""

            yield! context

        | Statement.While (condition, body) ->
            let! allocator = getFromContext (fun c -> c.Allocator)
            let startOfWhileLoop = allocator.AllocateLabel "while_start"
            let endOfWhileLoop = allocator.AllocateLabel "while_end"

            do! bprintfn $"%s{startOfWhileLoop}:"
            do! writeExpression condition
            do! bprintfn $"""
mov rax, 0
pop rax
cmp rax, 0
je ${endOfWhileLoop}
"""
            do! writeStatements body endOfFunctionLabel
            do! bprintfn $"""
jmp %s{startOfWhileLoop}
%s{endOfWhileLoop}:
"""

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

        | Statement.Expression (Expression.FuncCall (name, args) as expression) ->
            let! func = sourceContext (locateFunctionDecl name)
            let! expressionSize = sourceContext (getExpressionSize expression)
            do! bprintfn $"add rbp, %d{expressionSize}"

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
        let argsTypes = func.Args |> List.map snd
        let! argsSizes = argsTypes |> List.map (fun t -> getTypeIdSize { TypeId = t; Source = context.CurrentSource }) |> unwrapList
        let argsNames = func.Args |> List.map fst

        let stackEnv, _ =
            List.zip argsNames argsSizes
            |> List.fold (fun (env, offset) (name, size) ->
                let env = Map.add name offset env
                let offset = offset + size
                (env, offset)) (Map.empty, 8)

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

        if stackAllocator.TotalBytesAllocated > 65536 then
            yield! fatalDiag "Stack size exceeded 65536 bytes. 'enter' instruction can't take more bytes."

        fprintf $";;; %s{func.Name} ("
        let argsStr = func.Args |> List.map (fun (name, typ) -> sprintf $"%s{name} : %O{typ}") |> String.concat ", "
        fprintfn $"%s{argsStr}) : %O{func.ReturnType}"

        let funcLabel = getLabel' func.Name context.CurrentSource
        fprintfn $"""
%%push
static %s{funcLabel}
%s{funcLabel}:
enter %d{align stackAllocator.TotalBytesAllocated 8}, 0
;;; Body
%O{funcBodyStr}
;;; End of body
%s{endOfFunctionLabel}:
"""

        match TypeId.unwrapConst func.ReturnType with
        | TypeId.Void ->
            fprintfn $"""
leave
mov rax, 0
pop rax
add rsp, 8
add rsp, %d{List.sum argsSizes}
push rax
ret
%%pop
"""
        | typ ->
            let! size = getTypeIdSize { TypeId = typ; Source = context.CurrentSource }
            let again = stackAllocator.AllocateLabel "result_copying_again"
            let endLabel = stackAllocator.AllocateLabel "result_copying_end"

            match size with
            | 0 -> failwithf "Zero should not happen."
            | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 ->
                fprintfn """
mov rax, 0
mov rbx, 0
pop rax
leave
mov rbx, qword [rsp]
push rax
push rbx
ret
"""
            | size ->
            fprintfn $"""
mov rax, 0
mov rbx, 0
lea rax, qword [rsp]
leave
mov rbx, qword [rsp]
add rsp, 8
add rsp, %d{List.sum argsSizes}
mov rcx, 0
%s{again}:
cmp rcx, %d{size}
je %s{endLabel}
mov rdx, 0
mov dl, byte [rax+rcx]
mov byte [rsp+rcx], dl
add rcx, 1
jmp %s{again}
%s{endLabel}:
sub rsp, %d{size}
push rbx
ret
"""

        fprintfn "%%pop"
    }

    let writeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        match func.Modifier with
        | None -> do! writeNativeFunction func
        | Some Modifier.Export ->
            failwith "Exported functions to be implemented."
        | Some Modifier.Extern ->
            failwith "Extern functions to be implemented."
    }

    let runm (m: TypeCheckerM.M<SourceContext, unit>) (ctx: SourceContext) =
        match m ctx with
        | Ok ((), []) -> ()
        | Ok ((), xs) ->
            failwithf $"There are some errors running type checker context:\n%s{diags2Str xs}"
        | Error [] -> ()
        | Error xs ->
            failwithf $"There are some errors running type checker context:\n%s{diags2Str xs}"

    interface ICodegenerator with

        member _.Write() =
            for source in program.Sources do
                fprintf $";;; Source '%s{source.Filename}'\n\n"

                match makeContext source program with
                | Ok context ->
                    for func in context.CurrentSource |> getFunctionDeclarations do
                        runm (writeFunction func) context
                | Error diags ->
                    failwithf $"Error creating type checker context:\n%s{diags2Str diags}"

                fprintf $";;; End of source '%s{source.Filename}'\n\n"
