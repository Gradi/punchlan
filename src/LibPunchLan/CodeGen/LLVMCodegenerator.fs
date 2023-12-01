module LibPunchLan.CodeGen.LLVMCodegenerator

open System
open System.Globalization
open LibPunchLan.CodeGen
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking
open LibPunchLan.TypeChecking.TypeChecker
open LibPunchLan.TypeChecking.TypeCheckerM
open LibPunchLan.CodeGen.CodeUtils
open System.IO
open LibPunchLan.Comp
open System.Text
open LibPunchLan.Lexing
open Microsoft.FSharp.Reflection

type Defer =
    { Body: StringBuilder
      Value: string }

type StackAllocator () =
    let mutable valueCount = 0
    let mutable labelCount = 0
    let mutable defers = []

    member _.Defers = defers

    member _.AllocateValue () =
        let result = sprintf $"%%val%d{valueCount}"
        valueCount <- valueCount + 1
        result

    member _.AllocateLabel suffix =
        let result = sprintf $"lbl%d{labelCount}_%s{suffix}"
        labelCount <- labelCount + 1
        result

    member this.AllocateDefer () =
        let defer =  { Defer.Body = StringBuilder 8192; Value = this.AllocateValue () }
        defers <- defers @ [ defer ]
        defer

type FunctionArgInfo =
    { Function: Function
      Source: Source
      Index: int
      Name: string
      Type: TypeRef
      LlvmType: string }

type MemberInfo =
    { TypeDecl: TypeDeclRef
      Index: int
      Name: string
      Type: TypeRef
      LlvmType: string }

type CodegenContext =
    { SourceContext: SourceContext
      FunctionReturnLabel: string
      FunctionResultPtr: string
      Body: StringBuilder
      LocalsEnv: Map<string, string>
      Allocator: StackAllocator }

let getTypename (typename: string) (source: Source) =
    let filename = source.Filename.Replace ('/', '_')
    sprintf $"type___%s{filename}_%s{typename}"

let type2llvmtype (typeRef: TypeRef) : TypeCheckerM.M<SourceContext, string> = tchecker {
    match TypeId.unwrapConst typeRef.TypeId with
    | TypeId.Int8 | TypeId.Uint8 -> yield "i8"
    | TypeId.Int16 | TypeId.Uint16 -> yield  "i16"
    | TypeId.Int32 | TypeId.Uint32 -> yield "i32"
    | TypeId.Int64 | TypeId.Uint64 -> yield "i64"
    | TypeId.Float -> yield "float"
    | TypeId.Double -> yield "double"
    | TypeId.Bool -> yield "i1"
    | TypeId.Char -> yield "i8"
    | TypeId.Void -> yield "void"
    | TypeId.Pointer _ -> yield "ptr"
    | TypeId.Named typename -> yield getTypename typename.Name typeRef.Source
    | typ -> yield failwithf $"Type id '%O{typ}' should have been covered."
}

let type2llvmtype' (typeRef: TypeRef) : TypeCheckerM.M<CodegenContext, string> = tchecker {
    yield! checkWithContext' (fun c -> c.SourceContext) (type2llvmtype typeRef)
}

let expr2llvmtype (expr: Expression) : TypeCheckerM.M<CodegenContext, string> = tchecker {
    let! exprType = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expr)
    yield! type2llvmtype' exprType
}

let getFunctionArgInfo (func: Function) (source: Source) : TypeCheckerM.M<SourceContext, FunctionArgInfo list> = tchecker {
    yield!
        func.Args
        |> List.indexed
        |> List.map (fun (index, (argName, argType)) -> tchecker {
            let typeRef = { TypeId = argType; Source = source }
            let! llvmType = type2llvmtype typeRef
            yield { FunctionArgInfo.Function = func
                    Source = source
                    Index = index
                    Name = argName
                    Type = typeRef
                    LlvmType = llvmType }
            })
        |> unwrapList
}

let string2llvm (string: string) =
    let bytes = Encoding.UTF8.GetBytes string
    let bytes = Array.append bytes (Array.create 8 (byte 0))
    let hex = bytes |> Array.map (fun b -> sprintf $"i8 u0x%02x{b}") |> String.concat ", "
    sprintf $"[ %d{Array.length bytes} x i8 ] [ %s{hex} ]"

let getInternalName name (source: Source) =
    let filename = source.Filename.Replace ('/', '_')
    sprintf $"%s{filename}___%s{name}"

let printf str : TypeCheckerM.M<CodegenContext, unit> = tchecker {
    let! body = getFromContext (fun c -> c.Body)
    Printf.bprintf body str
}

let printfn str : TypeCheckerM.M<CodegenContext, unit> = tchecker {
    let! body = getFromContext (fun c -> c.Body)
    do! printf str
    Printf.bprintf body "\n"
}

let allocateValue () : TypeCheckerM.M<CodegenContext, string> = tchecker {
    yield! getFromContext (fun c -> c.Allocator.AllocateValue ())
}

let allocateLabel suffix : TypeCheckerM.M<CodegenContext, string> = tchecker {
    yield! getFromContext (fun c -> c.Allocator.AllocateLabel suffix)
}

let allocateDefer () : TypeCheckerM.M<CodegenContext, Defer> = tchecker {
    yield! getFromContext (fun c -> c.Allocator.AllocateDefer ())
}

let getMembers (expr: Expression) : TypeCheckerM.M<CodegenContext, MemberInfo list> = tchecker {
    let! exprType = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expr)
    match TypeId.unwrapConst exprType.TypeId with
    | TypeId.Named typename ->
        let! typeDecl = checkWithContext' (fun c -> { c.SourceContext with CurrentSource = exprType.Source }) (locateTypeDecl typename)

        yield!
            typeDecl.TypeDecl.Fields
            |> List.indexed
            |> List.map (fun (index, (name, typ)) -> tchecker {
                let typeRef = { TypeId = typ; Source = typeDecl.Source }
                let! llvmType = type2llvmtype' typeRef
                yield { MemberInfo.TypeDecl = typeDecl
                        Index = index
                        Name = name
                        Type = typeRef
                        LlvmType = llvmType }
            })
            |> unwrapList
    | typ -> yield failwithf $"Expression should have return aggregate type, but not '%O{typ}'."
}

let getMember (expr: Expression) (memberName: string) : TypeCheckerM.M<CodegenContext, MemberInfo> = tchecker {
    let! members = getMembers expr

    match members |> List.tryFind (fun m -> m.Name = memberName) with
    | Some mem -> yield mem
    | None ->
        let! context = getFromContext (fun c -> c.SourceContext)
        let! typ = checkWithContext context (getExpressionType expr)
        yield! checkWithContext context (fatalDiag $"Can't find member name '%s{memberName}' in '%O{typ}'.")
}

let number2llvm (number: Number) =
    let sign = NumberMod.getNumberSign number
    match NumberMod.unwrapNegative number with
    | Number.Integer decimals ->
        let decimals = decimals |> Array.map NumberMod.decIntToStr |> String.concat ""
        if sign >= 0 then decimals else sprintf $"(-%s{decimals})"
    | Number.HexInteger hexes ->
        let hexes = hexes |> Array.map NumberMod.hexIntToStr |> String.concat ""
        sprintf $"u0x%s{hexes}"
    | Number.BinaryInteger _ -> failwith "Converting binary integer to LLVM integer is not implemented."
    | Number.Double dbl -> sprintf $"%.10f{dbl}"
    | Number.Negative _ -> failwith "Negative number should have been removed at this point."

type LLVMCodegenerator (tw: TextWriter, program: Program) =

    let fprintf str = Printf.fprintf tw str

    let fprintfn str =
        fprintf str
        fprintf "\n"

    let rec writeExpression (expression: Expression) : TypeCheckerM.M<CodegenContext, string> = tchecker {
        let! context = context
        let! exprLlvmType = expr2llvmtype expression

        match expression with
        | Expression.Constant (Value.String string) -> yield sprintf $"@%s{string2label string}"
        | Expression.Constant (Value.Number number) -> yield number2llvm number
        | Expression.Constant (Value.Boolean bool) -> yield bool.ToString ()
        | Expression.Constant (Value.Char ch) -> yield sprintf $"u0x%02x{int ch}"

        | Expression.Variable varname ->
            let! value = allocateValue ()
            match Map.tryFind varname context.LocalsEnv with
            | Some ptrvalue ->
                do! printfn $"%s{value} = load %s{exprLlvmType}, ptr %s{ptrvalue}"
            | None ->
                let! varDecl = checkWithContext' (fun c -> c.SourceContext) (locateVariableDecl { Name = varname; Alias = None })
                let varname =
                    match varDecl.Variable.Modifier with
                    | None -> getInternalName varDecl.Variable.Name varDecl.Source
                    | Some Modifier.Export
                    | Some Modifier.Extern -> varDecl.Variable.Name

                do! printfn $"%s{value} = load %s{exprLlvmType}, ptr @%s{varname}"
            yield value

        | Expression.FuncCall (funcName, args) ->
            let! func = checkWithContext' (fun c -> c.SourceContext) (locateFunctionDecl funcName)
            let! result = allocateValue ()
            let name =
                match func.Function.Modifier with
                | None -> getInternalName func.Function.Name func.Source
                | Some Modifier.Export
                | Some Modifier.Extern -> func.Function.Name

            let! argsTypes =
                args
                |> List.map expr2llvmtype
                |> unwrapList

            let! executedArgs =
                args
                |> List.map writeExpression
                |> unwrapList

            if TypeId.isVoid func.Function.ReturnType then
                do! printf $"call ccc %s{exprLlvmType} @%s{name} ("
            else
                do! printf $"%s{result} = call ccc %s{exprLlvmType} @%s{name} ("
            for index, (argType, argValue) in List.indexed (List.zip argsTypes executedArgs) do
                if index > 0 then do! printf ", "
                do! printf $"%s{argType} %s{argValue}"
            do! printfn ")"
            yield result

        | Expression.MemberAccess (Expression.Variable variablename as varexpr, membername)
            when isStructVariableWithMember variablename membername context.SourceContext ->

            let! llvmStructType = expr2llvmtype varexpr
            let! memberInfo = getMember varexpr membername
            let! ptr = allocateValue ()
            let! result = allocateValue ()

            let! address = writeExpressionAddress varexpr
            do! printfn $"%s{ptr} = getelementptr %s{llvmStructType}, ptr %s{address}, i64 %d{memberInfo.Index}"
            do! printfn $"%s{result} = load %s{exprLlvmType}, ptr %s{ptr}"
            yield result

        | Expression.MemberAccess (Expression.Variable alias, membername) when getAliasedSource alias context.SourceContext |> Result.isOk ->
            let! varDecl = checkWithContext' (fun c -> c.SourceContext) (locateVariableDecl { Name = membername; Alias = Some alias })
            let name =
                match varDecl.Variable.Modifier with
                | None -> getInternalName varDecl.Variable.Name varDecl.Source
                | Some Modifier.Extern
                | Some Modifier.Export -> varDecl.Variable.Name

            let! llvmType = type2llvmtype' { TypeId = varDecl.Variable.TypeId; Source = varDecl.Source }
            let! result = allocateValue ()

            do! printfn $"%s{result} = load %s{llvmType}, ptr @%s{name}"
            yield result

        | Expression.MemberAccess (left, membername) ->
            let! structType = expr2llvmtype left
            let! memberInfo = getMember left membername
            let! result = allocateValue ()

            let! leftResult = writeExpression left
            do! printfn $"%s{result} = extractvalue %s{structType}, %d{memberInfo.Index}"
            yield result

        | Expression.BinaryExpression { Left = leftexpr; Right = rightexpr; Kind = kind } ->
            let! leftType = checkWithContext context.SourceContext (getExpressionType leftexpr)
            let! rightType = checkWithContext context.SourceContext (getExpressionType rightexpr)
            let lt = leftType.TypeId
            let rt = rightType.TypeId
            let! llvmtype = type2llvmtype' leftType
            let! rllvmtype = type2llvmtype' rightType

            let! lefttmp = allocateValue ()
            let! righttmp = allocateValue ()
            let! result = allocateValue ()

            let fail op = checkWithContext context.SourceContext (fatalDiag $"Operation '%O{lt}' %s{op} '%O{rt}' is not covered.")


            match kind with
            | BinaryExpressionKind.Plus
            | BinaryExpressionKind.Minus ->
                let iop = if kind = BinaryExpressionKind.Plus then "add" else "sub"
                let fop = if kind = BinaryExpressionKind.Plus then "fadd" else "fsub"

                if TypeId.isUnsigned lt && TypeId.isUnsigned rt then

                    let! lefttmp = zext leftexpr
                    let! righttmp = zext rightexpr
                    do! printfn $"%s{result} = %s{iop} i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isSigned lt && TypeId.isSigned rt then

                    let! lefttmp = sext leftexpr
                    let! righttmp = sext rightexpr
                    do! printfn $"%s{result} = %s{iop} i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isPointer lt && TypeId.isUnsigned rt then

                    let! tmpResult = allocateValue ()
                    let! left = writeExpression leftexpr
                    let! righttmp = zext rightexpr
                    do! printfn $"%s{lefttmp} = ptrtoint ptr %s{left} to i64"
                    do! printfn $"%s{tmpResult} = %s{iop} i64 %s{lefttmp}, %s{righttmp}"
                    do! printfn $"%s{result} = inttoptr i64 %s{tmpResult} to ptr"

                elif TypeId.isUnsigned lt && TypeId.isPointer rt then

                    let! tmpResult = allocateValue ()
                    let! lefttmp = zext leftexpr
                    let! right = writeExpression rightexpr
                    do! printfn $"%s{righttmp} = ptrtoint ptr %s{right} to i64"
                    do! printfn $"%s{tmpResult} = %s{iop} i64 %s{lefttmp}, %s{righttmp}"
                    do! printfn $"%s{result} = inttoptr i64 %s{tmpResult} to ptr"

                elif TypeId.isFloat lt && TypeId.isFloat rt then

                    let! leftfp = fpext leftexpr
                    let! rightfp = fpext rightexpr
                    do! printfn $"%s{result} = %s{fop} double %s{leftfp}, %s{rightfp}"

                else
                    yield! fail "(+ -)"

            | BinaryExpressionKind.Multiply ->

                if TypeId.isUnsigned lt && TypeId.isUnsigned rt then

                    let! lefttmp = zext leftexpr
                    let! righttmp = zext rightexpr
                    do! printfn $"%s{result} = mul i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isSigned lt && TypeId.isSigned rt then

                    let! lefttmp = sext leftexpr
                    let! righttmp = sext rightexpr
                    do! printfn $"%s{result} = mul i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isFloat lt && TypeId.isFloat rt then

                    let! leftfp = fpext leftexpr
                    let! rightfp = fpext rightexpr
                    do! printfn $"%s{result} = fmul double %s{leftfp}, %s{rightfp}"

                else
                    yield! fail "*"

            | BinaryExpressionKind.Division ->

                if TypeId.isUnsigned lt && TypeId.isUnsigned rt then

                    let! lefttmp = zext leftexpr
                    let! righttmp = zext rightexpr
                    do! printfn $"%s{result} = udiv i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isSigned lt && TypeId.isSigned rt then

                    let! lefttmp = sext leftexpr
                    let! righttmp = sext rightexpr
                    do! printfn $"%s{result} = sdiv i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isFloat lt && TypeId.isFloat rt then

                    let! leftfp = fpext leftexpr
                    let! rightfp = fpext rightexpr
                    do! printfn $"%s{result} = fdiv double %s{leftfp}, %s{rightfp}"

                else
                    yield! fail "/"

            | BinaryExpressionKind.Equal
            | BinaryExpressionKind.NotEqual ->
                let op = if kind = BinaryExpressionKind.Equal then "eq" else "ne"

                if TypeId.isPointer lt && TypeId.isUnsigned rt then
                    let! left = writeExpression leftexpr
                    do! printfn $"%s{lefttmp} = ptrtoint ptr %s{left} to i64"

                    let! righttmp = zext rightexpr

                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isUnsigned lt && TypeId.isPointer rt then

                    let! lefttmp = zext leftexpr

                    let! right = writeExpression rightexpr
                    do! printfn $"%s{righttmp} = ptrtoint ptr %s{right} to i64"

                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isPointer lt && TypeId.isPointer rt then

                    let! left = writeExpression leftexpr
                    let! right = writeExpression rightexpr

                    do! printfn $"%s{lefttmp} = ptrtoint ptr %s{left} to i64"
                    do! printfn $"%s{righttmp} = ptrtoint ptr %s{right} to i64"
                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"

                else
                    let! lefttmp = zext leftexpr
                    let! righttmp = zext rightexpr

                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"


            | BinaryExpressionKind.Less
            | BinaryExpressionKind.LessOrEqual
            | BinaryExpressionKind.Greater
            | BinaryExpressionKind.GreaterOrEqual ->

                if TypeId.isUnsigned lt && TypeId.isUnsigned rt then
                    let! lefttmp = zext leftexpr
                    let! righttmp = zext rightexpr
                    let op =
                        match kind with
                        | BinaryExpressionKind.Less -> "ult"
                        | BinaryExpressionKind.LessOrEqual -> "ule"
                        | BinaryExpressionKind.Greater -> "ugt"
                        | _ -> "uge"
                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"

                elif TypeId.isSigned lt && TypeId.isSigned rt then
                    let! lefttmp = sext leftexpr
                    let! righttmp = sext rightexpr
                    let op =
                        match kind with
                        | BinaryExpressionKind.Less -> "slt"
                        | BinaryExpressionKind.LessOrEqual -> "sle"
                        | BinaryExpressionKind.Greater -> "sgt"
                        | _ -> "sge"
                    do! printfn $"%s{result} = icmp %s{op} i64 %s{lefttmp}, %s{righttmp}"
                else
                    yield! fail "(< <= > >=)"

            | BinaryExpressionKind.Or ->

                if TypeId.isBool lt && TypeId.isBool rt then

                    let! orLhs = allocateLabel "or_lhs"
                    let! orRhs = allocateLabel "or_rhs"
                    let! orEndBranch = allocateLabel "or_end"
                    let! leftCmpResult = allocateValue ()
                    let! rightCmpResult = allocateValue ()

                    do! printfn $"br label %%%s{orLhs}"
                    do! printfn $"%s{orLhs}:"
                    let! left = writeExpression leftexpr
                    do! printfn $"%s{leftCmpResult} = icmp eq i1 %s{left}, 1"
                    do! printfn $"br i1 %s{leftCmpResult}, label %%%s{orEndBranch}, label %%%s{orRhs}"
                    do! printfn $"%s{orRhs}:"
                    let! right = writeExpression rightexpr
                    do! printfn $"%s{rightCmpResult} = icmp eq i1 %s{right}, 1"
                    do! printfn $"br label %%%s{orEndBranch}"
                    do! printfn $"%s{orEndBranch}:"
                    do! printfn $"%s{result} = phi i1 [%s{leftCmpResult}, %%%s{orLhs}], [%s{rightCmpResult}, %%%s{orRhs}]"

                else
                    let! lefttmp, righttmp = tchecker {
                        if TypeId.isUnsigned lt && TypeId.isUnsigned rt then
                            let! lefttmp = zext leftexpr
                            let! righttmp = zext rightexpr
                            yield (lefttmp, righttmp)
                        elif TypeId.isSigned lt && TypeId.isSigned rt then
                            let! lefttmp = sext leftexpr
                            let! righttmp = sext rightexpr
                            yield (lefttmp, righttmp)
                        else
                            yield! fail "or"
                    }

                    do! printfn $"%s{result} = or i64 %s{lefttmp}, %s{righttmp}"

            | BinaryExpressionKind.And ->

                if TypeId.isBool lt && TypeId.isBool rt then

                    let! andLhs = allocateLabel "and_lhs"
                    let! andRhs = allocateLabel "and_rhs"
                    let! andEnd = allocateLabel "and_end"

                    do! printfn $"br label %%%s{andLhs}"
                    do! printfn $"%s{andLhs}:"
                    let! left = writeExpression leftexpr
                    do! printfn $"br i1 %s{left}, label %%%s{andRhs}, label %%%s{andEnd}"
                    do! printfn $"%s{andRhs}:"
                    let! right = writeExpression rightexpr
                    do! printfn $"br label %%%s{andEnd}"
                    do! printfn $"%s{andEnd}:"
                    do! printfn $"%s{result} = phi i1 [%s{left}, %%%s{andLhs}], [%s{right}, %%%s{andRhs}]"

                else

                    let! lefttmp, righttmp = tchecker {
                        if TypeId.isUnsigned lt && TypeId.isUnsigned rt then
                            let! lefttmp = zext leftexpr
                            let! righttmp = zext rightexpr
                            yield (lefttmp, righttmp)
                        elif TypeId.isSigned lt && TypeId.isSigned rt then
                            let! lefttmp = sext leftexpr
                            let! righttmp = sext rightexpr
                            yield (lefttmp, righttmp)
                        else
                            yield! fail "and"
                    }

                    do! printfn $"%s{result} = and i64 %s{lefttmp}, %s{righttmp}"

            | BinaryExpressionKind.Xor
            | BinaryExpressionKind.RShift
            | BinaryExpressionKind.LShift ->

                let! lefttmp, righttmp = tchecker {
                    if TypeId.isUnsigned lt && TypeId.isUnsigned rt then
                        let! lefttmp = zext leftexpr
                        let! righttmp = zext rightexpr
                        yield (lefttmp, righttmp)
                    elif TypeId.isSigned lt && TypeId.isSigned rt then
                        let! lefttmp = sext leftexpr
                        let! righttmp = sext rightexpr
                        yield (lefttmp, righttmp)
                    else
                        yield! fail "(xor >> <<)"
                }

                match kind with
                | BinaryExpressionKind.Xor ->
                    do! printfn $"%s{result} = xor i64 %s{lefttmp}, %s{righttmp}"
                | BinaryExpressionKind.RShift ->
                    do! printfn $"%s{result} = lshr i64 %s{lefttmp}, %s{righttmp}"
                | _ ->
                    do! printfn $"%s{result} = shl i64 %s{lefttmp}, %s{righttmp}"

            yield result

        | Expression.ArrayAccess (arrayexpr, indexexpr) ->
            let! array = writeExpression arrayexpr
            let! index = writeExpression indexexpr
            let! result = allocateValue ()
            let! tmpPtr = allocateValue ()
            do! printfn $"%s{tmpPtr} = getelementptr %s{exprLlvmType}, ptr %s{array}, i64 %s{index}"
            do! printfn $"%s{result} = load %s{exprLlvmType}, ptr %s{tmpPtr}"
            yield result

        | Expression.StructCreation _ -> yield failwith "Struct expression not implemented."

        | Expression.Bininversion expr ->
            let! exprType = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expr)
            let! exprllvmtype = type2llvmtype' exprType

            let! result = allocateValue ()

            let! extendedValue = tchecker {
                if TypeId.isUnsigned exprType.TypeId then
                    yield! zext expr
                elif TypeId.isSigned exprType.TypeId then
                    yield! sext expr
                else
                    yield! checkWithContext' (fun c -> c.SourceContext) (fatalDiag $"Binary inversion of '%O{exprType}' is not covered.")
            }

            let! bitReverseValue = allocateValue ()
            do! printfn $"%s{bitReverseValue} = xor i64 %s{extendedValue}, -1"
            do! printfn $"%s{result} = trunc i64 %s{bitReverseValue} to %s{exprllvmtype}"
            yield result

        | Expression.Sizeof typeid ->
            let! size = checkWithContext' (fun c -> c.SourceContext) (getTypeIdSize { TypeId = typeid; Source = context.SourceContext.CurrentSource })
            yield sprintf $"%d{size}"

        | Expression.Addrof expr -> yield! writeExpressionAddress expr

        | Expression.Deref expr ->
            let! ptr = writeExpression expr
            let! result = allocateValue ()
            do! printfn $"%s{result} = load %s{exprLlvmType}, ptr %s{ptr}"
            yield result

        | Expression.Cast (target, expr) ->
            let target = { TypeId = target; Source = context.SourceContext.CurrentSource }
            let! source = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expr)
            let! result = allocateValue ()
            let! targetllvmtype = type2llvmtype' target
            let! sourcellvmtype = type2llvmtype' source

            do! printfn $"; Cast from '%O{source.TypeId}' to '%O{target.TypeId}'"

            let! sourceExpr = writeExpression expr

            if TypeId.isIntegerType source.TypeId && TypeId.isIntegerType target.TypeId then
                let! sourceSize = checkWithContext' (fun c -> c.SourceContext) (getTypeIdSize source)
                let! targetSize = checkWithContext' (fun c -> c.SourceContext) (getTypeIdSize target)

                if sourceSize < targetSize then
                    do! printfn $"%s{result} = zext %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"
                elif sourceSize = targetSize then
                    do! printfn $"%s{result} = bitcast %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"
                else
                    do! printfn $"%s{result} = trunc %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"

            elif TypeId.isUnsigned source.TypeId && TypeId.isFloat target.TypeId then
                do! printfn $"%s{result} = uitofp %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"

            elif TypeId.isSigned source.TypeId && TypeId.isFloat target.TypeId then
                do! printfn $"%s{result} = sitofp %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"

            elif TypeId.isFloat source.TypeId && TypeId.isUnsigned target.TypeId then
                do! printfn $"%s{result} = fptoui %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"

            elif TypeId.isFloat source.TypeId && TypeId.isSigned target.TypeId then
                do! printfn $"%s{result} = fptosi %s{sourcellvmtype} %s{sourceExpr} to %s{targetllvmtype}"

            elif TypeId.isPointer source.TypeId && TypeId.isPointer target.TypeId then
                do! printfn $"%s{result} = bitcast ptr %s{sourceExpr} to ptr"

            else
                yield! checkWithContext' (fun c -> c.SourceContext) (fatalDiag $"Cast from '%O{source}' to '%O{target}' is not covered.")

            yield result
    }

    and writeExpressionAddress (expression: Expression) : TypeCheckerM.M<CodegenContext, string> = tchecker {
        let! context = context

        match expression with
        | Variable varname ->
            match Map.tryFind varname context.LocalsEnv with
            | Some ptr -> yield ptr
            | None ->
                let! varDecl = checkWithContext' (fun c -> c.SourceContext) (locateVariableDecl { Name = varname; Alias = None })
                let name =
                    match varDecl.Variable.Modifier with
                    | None -> getInternalName varDecl.Variable.Name varDecl.Source
                    | Some Modifier.Export
                    | Some Modifier.Extern -> varDecl.Variable.Name
                yield sprintf $"@%s{name}"

        | ArrayAccess (array, index) ->
            let! arrayType = checkWithContext' (fun c -> c.SourceContext) (getExpressionType array)
            let! arraySubitemType = checkWithContext' (fun c -> c.SourceContext) (getArraySubitemType arrayType.TypeId)
            let! arraySubitemLLvmType = type2llvmtype' { TypeId = arraySubitemType; Source = arrayType.Source }
            let! result = allocateValue ()

            let! array = writeExpression array
            let! index = writeExpression index
            do! printfn $"%s{result} = getelementptr %s{arraySubitemLLvmType}, ptr %s{array}, i64 %s{index}"
            yield result

        | expression ->
            let union, _ = FSharpValue.GetUnionFields (expression, expression.GetType ())
            yield! checkWithContext' (fun c -> c.SourceContext) (fatalDiag $"Can't get address of '%s{union.Name}' expression.")
    }

    and extend (expression: Expression) (op: string) : TypeCheckerM.M<CodegenContext, string> = tchecker {
        let! exprType = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expression)
        let! llvmtype = type2llvmtype' exprType

        let! target = writeExpression expression

        match TypeId.unwrapConst exprType.TypeId with
        | TypeId.Int8 | TypeId.Uint8
        | TypeId.Int16 | TypeId.Uint16
        | TypeId.Int32 | TypeId.Uint32
        | TypeId.Bool | TypeId.Char ->
            let! result = allocateValue ()
            do! printfn $"%s{result} = %s{op} %s{llvmtype} %s{target} to i64"
            yield result
        | TypeId.Int64 | TypeId.Uint64 -> yield target
        | typ -> yield failwithf $"Can't extend (%s{op}) type '%O{typ}'."
    }

    and zext expression = extend expression "zext"

    and sext expression = extend expression "sext"

    and fpext (expression: Expression) : TypeCheckerM.M<CodegenContext, string> = tchecker {
        let! typ = checkWithContext' (fun c -> c.SourceContext) (getExpressionType expression)

        match TypeId.unwrapConst typ.TypeId with
        | TypeId.Float ->
            let! expr = writeExpression expression
            let! result = allocateValue ()
            do! printfn $"%s{result} = fpext float %s{expr} to double"
            yield result
        | TypeId.Double -> yield! writeExpression expression
        | typ -> yield failwithf $"Can't fpext type '%O{typ}'."
    }

    let rec writeStatements (statements: Statement list) : TypeCheckerM.M<CodegenContext, unit> = tchecker {
        let folder context statement = (fun () -> tchecker {
            let! context = context ()
            yield! checkWithContext context (writeStatement statement)
        })

        let bigAction = statements |> List.fold folder (fun () -> context)
        let! _ = bigAction ()
        yield ()
    }

    and writeStatement (statement: Statement) : TypeCheckerM.M<CodegenContext, CodegenContext> = tchecker {
        match statement with
        | Statement.VarDecl (varName, varType, initExpr) ->
            let! variablePtr = allocateValue ()
            let! context = context
            let typeRef = { TypeId = varType; Source = context.SourceContext.CurrentSource }
            let! llvmType = type2llvmtype' typeRef
            let nameTypeEnv = lazy (Map.add varName typeRef context.SourceContext.NameTypeEnv.Value )
            let newContext = { context with LocalsEnv = Map.add varName variablePtr context.LocalsEnv
                                            SourceContext = { context.SourceContext with NameTypeEnv = nameTypeEnv}}

            do! printfn $"%s{variablePtr} = alloca %s{llvmType}, i64 1"

            match initExpr with
            | None -> yield newContext
            | Some expr ->
                let! expr = writeExpression expr
                do! printfn $"store %s{llvmType} %s{expr}, ptr %s{variablePtr}"
                yield newContext

        | Statement.VarAssignment (left, right) ->
            let! leftAddress = writeExpressionAddress left
            let! rightType = expr2llvmtype right
            let! right = writeExpression right

            do! printfn $"store %s{rightType} %s{right}, ptr %s{leftAddress}"
            yield! context

        | Statement.If (mainCond, elseIfs, elses) ->
            let! ifEnd = allocateLabel "if_global_end"

            let writeIf (ifCond: IfCond) elseBranch = tchecker {
                let! ifBody = allocateLabel "if_body"

                let! boolValue = writeExpression ifCond.Condition
                do! printfn $"br i1 %s{boolValue}, label %%%s{ifBody}, label %%%s{elseBranch}"
                do! printfn $"%s{ifBody}:"
                do! writeStatements ifCond.Body
                do! printfn $"br label %%%s{ifEnd}"
            }

            let! elseBranch = allocateLabel "if_else"

            do! writeIf mainCond elseBranch

            let! finalElseBranch =
                elseIfs
                |> List.fold (fun prevBranch ifCond -> tchecker {
                    let! prevBranch = prevBranch
                    let! nextBranch = allocateLabel "if_else"

                    do! printfn $"%s{prevBranch}:"
                    do! writeIf ifCond nextBranch
                    yield nextBranch
                }) (tchecker { yield elseBranch })

            do! printfn $"%s{finalElseBranch}:"
            do! writeStatements elses
            do! printfn $"br label %%%s{ifEnd}"
            do! printfn $"%s{ifEnd}:"
            yield! context

        | Statement.For (indexVariable, startExpr, endExpr, stepExpr, body) ->
            let stepExpr = stepExpr |> Option.defaultValue (Expression.Constant (Value.Number (Number.Integer [| DecInt.One |])))
            let! forStart = allocateLabel "for_start"
            let! forCompare = allocateLabel "for_cmp"
            let! forBody = allocateLabel "for_body"
            let! forEnd = allocateLabel "for_end"

            // Start
            do! printfn $"br label %%%s{forStart}"
            do! printfn $"%s{forStart}:"

            // Declare index variable
            let! newContext = writeStatement (Statement.VarDecl (indexVariable, TypeId.Int64, Some startExpr))

            do! (checkWithContext newContext) (tchecker {
                let indexVariable = Expression.Variable indexVariable
                // Compare value
                do! printfn $"br label %%%s{forCompare}"
                do! printfn $"%s{forCompare}:"
                let! cmpResult = writeExpression (Expression.BinaryExpression { Left = indexVariable; Right = endExpr; Kind = BinaryExpressionKind.Less })
                do! printfn $"br i1 %s{cmpResult}, label %%%s{forBody}, label %%%s{forEnd}"

                // Body
                do! printfn $"%s{forBody}:"
                do! writeStatements body

                // Increment index
                let! _ = writeStatement (Statement.VarAssignment (indexVariable, Expression.BinaryExpression { Left = indexVariable; Right = stepExpr; Kind = BinaryExpressionKind.Plus }))
                do! printfn $"br label %%%s{forCompare}"

                do! printfn $"br label %%%s{forEnd}"
                do! printfn $"%s{forEnd}:"
            })

            yield! context

        | Statement.While (condition, body) ->
            let! whileStart = allocateLabel "while_start"
            let! whileBody = allocateLabel "while_body"
            let! whileEnd = allocateLabel "while_end"

            do! printfn $"br label %%%s{whileStart}"
            do! printfn $"%s{whileStart}:"
            let! result = writeExpression condition
            do! printfn $"br i1 %s{result}, label %%%s{whileBody}, label %%%s{whileEnd}"
            do! printfn $"%s{whileBody}:"
            do! writeStatements body
            do! printfn $"br label %%%s{whileStart}"
            do! printfn $"%s{whileEnd}:"
            yield! context

        | Statement.Defer body ->
            match body with
            | [] -> yield! context
            | body ->
                let! defer = allocateDefer ()
                do! printfn $"store i1 1, ptr %s{defer.Value}"
                do! checkWithContext' (fun c -> { c with Body = defer.Body }) (writeStatements body)
                yield! context

        | Statement.Return ->
            let! endLabel = getFromContext (fun c -> c.FunctionReturnLabel)
            do! printfn $"br label %%%s{endLabel}"
            yield! context

        | Statement.ReturnExpr expr ->
            let! context = context
            let! llvmtype = expr2llvmtype expr
            let! value = writeExpression expr
            do! printfn $"store %s{llvmtype} %s{value}, ptr %s{context.FunctionResultPtr}"
            do! printfn $"br label %%%s{context.FunctionReturnLabel}"
            yield context

        | Statement.Expression (Expression.FuncCall _ as expr) ->
            let! _ = writeExpression expr
            yield! context

        | Statement.Expression expression ->
            let union, _ = FSharpValue.GetUnionFields (expression, expression.GetType())
            yield! checkWithContext' (fun c -> c.SourceContext) (fatalDiag $"Expression '%s{union.Name}' can't be used as statement.")
    }

    let writeNativeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! context = context
        let! returnType = type2llvmtype { TypeId = func.ReturnType; Source = context.CurrentSource }
        let! arguments = getFunctionArgInfo func context.CurrentSource
        let allocator = StackAllocator ()

        let visibility, funcname =
            match func.Modifier with
            | None -> ("private", getInternalName func.Name context.CurrentSource)
            | Some Modifier.Export -> ("", func.Name)
            | Some Modifier.Extern -> failwithf "extern functions should be handled differently."

        fprintf $"define %s{visibility} ccc %s{returnType} @%s{funcname} ("
        for arg in arguments do
            if arg.Index > 0 then fprintf ", "
            fprintf $"%s{arg.LlvmType} %%%s{arg.Name}"
        fprintfn ") alignstack(16) nounwind {"
        fprintfn "enter:"
        fprintf "\n"

        let localsEnv =
            arguments
            |> List.fold (fun env argument -> Map.add argument.Name (allocator.AllocateValue ()) env) Map.empty

        for arg in arguments do
            let value = Map.find arg.Name localsEnv
            fprintfn $"%s{value} = alloca %s{arg.LlvmType}, i64 1"
        for arg in arguments do
            let value = Map.find arg.Name localsEnv
            fprintfn $"store %s{arg.LlvmType} %%%s{arg.Name}, ptr %s{value}"
        fprintf "\n"

        let functionResultPtr =
            if TypeId.isVoid func.ReturnType then ""
            else
                let value = allocator.AllocateValue ()
                fprintfn $"%s{value} = alloca %s{returnType}, i64 1 ; Function result goes here"
                fprintf "\n"
                value

        let nameTypeEnv = lazy (
                arguments
                |> List.fold (fun env arg -> Map.add arg.Name arg.Type env) context.NameTypeEnv.Value
            )
        let context = { context with NameTypeEnv = nameTypeEnv }
        let codegenContext =
            { SourceContext = context
              FunctionReturnLabel = allocator.AllocateLabel "function_end"
              FunctionResultPtr = functionResultPtr
              Body = StringBuilder 8192
              LocalsEnv = localsEnv
              Allocator = allocator }

        do! checkWithContext codegenContext (writeStatements func.Body)

        if not (List.isEmpty allocator.Defers) then
            fprintfn ";;; Defers check variables"
            for defer in allocator.Defers do
                fprintfn $"%s{defer.Value} = alloca i1, i64 1"
            for defer in allocator.Defers do
                fprintfn $"store i1 0, ptr %s{defer.Value}"
            fprintfn ";;; End of defers check variables"
            fprintf "\n"

        fprintfn ";;; Body"
        fprintf $"%O{codegenContext.Body}"
        fprintfn ";;; End of body"
        fprintfn $"br label %%%s{codegenContext.FunctionReturnLabel}"
        fprintf "\n"

        fprintfn $"%s{codegenContext.FunctionReturnLabel}:"
        fprintf "\n"

        if not (List.isEmpty allocator.Defers) then
            fprintfn ";;; Defers go here"
            for defer in List.rev allocator.Defers do
                let deferVar = allocator.AllocateValue ()
                let cmpResult = allocator.AllocateValue ()
                let deferBodyLabel = allocator.AllocateLabel "defer_body"
                let deferEndLabel = allocator.AllocateLabel "defer_end"

                fprintfn $"%s{deferVar} = load i1, ptr %s{defer.Value}"
                fprintfn $"%s{cmpResult} = icmp eq i1 %s{deferVar}, 1"
                fprintfn $"br i1 %s{cmpResult}, label %%%s{deferBodyLabel}, label %%%s{deferEndLabel}"
                fprintfn $"%s{deferBodyLabel}:"
                fprintf $"%O{defer.Body}"
                fprintfn $"br label %%%s{deferEndLabel}"
                fprintfn $"%s{deferEndLabel}:"
            fprintfn ";;; End of defers"
            fprintf "\n"

        match TypeId.isVoid func.ReturnType with
        | true ->
            fprintfn "ret void"
        | false ->
            let value = allocator.AllocateValue ()
            fprintfn $"%s{value} = load %s{returnType}, ptr %s{codegenContext.FunctionResultPtr}"
            fprintfn $"ret %s{returnType} %s{value}"

        fprintfn "}"
        fprintf "\n"
    }

    let writeExternFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        assert (func.Modifier |> Option.exists (fun v -> v = Modifier.Extern))

        let! context = context
        let! arguments = getFunctionArgInfo func context.CurrentSource
        let! returnType = type2llvmtype { TypeId = func.ReturnType; Source = context.CurrentSource }

        fprintf $"declare ccc %s{returnType} @%s{func.Name} ("
        for arg in arguments do
            if arg.Index > 0 then fprintf ", "
            fprintf $"%s{arg.LlvmType}"
        fprintf ") nounwind"
        fprintf "\n"
    }

    let writeFunction (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        match func.Modifier with
        | None
        | Some Modifier.Export ->
            do! writeNativeFunction func
        | Some Modifier.Extern ->
            do! writeExternFunction func
    }

    let writeVariable (var: Variable) : TypeCheckerM.M<SourceContext, unit> = tchecker {
        let! context = context

        let linkage, name =
            match var.Modifier with
            | None -> ("private", getInternalName var.Name context.CurrentSource)
            | Some Modifier.Export
            | Some Modifier.Extern -> ("", var.Name)

        let constGlobal =
            if TypeId.isConst var.TypeId then "const" else "global"

        let! llvmtype = type2llvmtype { TypeId = var.TypeId; Source = context.CurrentSource }

        fprintfn $"@%s{name} = %s{linkage} %s{constGlobal} %s{llvmtype} zeroinitializer, align 16"
        if Option.isSome var.InitExpr then
            failwith "Variables with init expressions aren't supported in LLVM codegen."

        fprintf "\n"
    }

    interface ICodegenerator with

        member _.Write () =

            // Intrinsics
            fprintfn "declare i64 @llvm.bitreverse.i64(i64)"
            fprintf "\n\n\n"

            for source in program.Sources do
                let context = makeContext' source program

                fprintfn $";;; Source '%s{source.Filename}'\n"
                fprintfn ";;; Functions "
                for func in getFunctionDeclarations source do
                    runtc (writeFunction func) context

                fprintfn ";;; Variables"
                for variable in getVariableDeclarations source do
                    runtc (writeVariable variable) context

                fprintfn $";;; End of Source '%s{source.Filename}'\n"



            let strings =
                program.Sources
                |> Seq.ofList
                |> Seq.collect getStringsFromSource
                |> Seq.map (fun str -> (string2label str, string2llvm str))
                |> Seq.distinctBy fst
                |> List.ofSeq

            if not (List.isEmpty strings) then
                fprintfn ";;; Strings \n"
                for label, value in strings do
                    fprintfn $"@%s{label} = private constant %s{value}, align 16\n"
                fprintfn ";;; End of Strings\n"
