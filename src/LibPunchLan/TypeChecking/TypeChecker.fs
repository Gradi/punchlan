module LibPunchLan.TypeChecking.TypeChecker

open LibPunchLan
open LibPunchLan.TypeChecking.TypeCheckerM
open LibPunchLan.Comp
open LibPunchLan.Parsing
open LibPunchLan.Lexing

let checkOpenDirectives () : TypeCheckerM.M<SourceContext, unit> = tchecker {
    let! odis = getFromContext (fun c -> c.CurrentSource.OpenDirectives)

    let paths = odis |> List.map (fun od -> od.Path) |> MList.duplicates
    let aliases = odis |> List.map (fun od -> od.Alias) |> List.choose id |> MList.duplicates

    if not (List.isEmpty paths) then yield! diag $"Source contains duplicated open paths: %A{paths}"
    if not (List.isEmpty aliases) then yield! diag $"Source contains duplicated open aliases: %A{aliases}"

    let! sourceFilename = getFromContext (fun c -> c.CurrentSource.Filename)
    if odis |> List.exists (fun od -> od.Path = sourceFilename) then
        yield! diag "Source can't 'open' itself."
}

let checkUniqueDeclarations () : TypeCheckerM.M<SourceContext, unit> = tchecker {
    let! source = getFromContext (fun c -> c.CurrentSource)

    let varsAndFuncs =
        source |> getVariableDeclarations
        |> List.map (fun v -> v.Name)
        |> List.append (source |> getFunctionDeclarations |> List.map (fun f -> f.Name))
        |> MList.duplicates

    let types =
        source |> getTypeDeclarations
        |> List.map (fun t -> t.Name)
        |> MList.duplicates

    if not (List.isEmpty varsAndFuncs) then yield! diag $"Source contains duplicated variable and function names: %A{varsAndFuncs}"
    if not (List.isEmpty types) then yield! diag $"Source contains duplicated type names: %A{types}"
}

let checkNamedTypeIdExists (typ: TypeId) : TypeCheckerM.M<SourceContext, unit> = tchecker {
    match TypeId.unwrapPointerAndConst typ with
    | Named name ->
        let! _ = locateTypeDecl name
        yield ()
    | _ -> yield ()
}

let rec isExpressionConstant (expr: Expression) =
    match expr with
    | Constant _ -> true
    | StructCreation (_, fields) -> fields |> List.map snd |> List.forall isExpressionConstant
    | _ -> false

let getArraySubitemType (typ: TypeId) : TypeCheckerM.M<SourceContext, TypeId> = tchecker {
    match typ with
    | TypeId.Pointer typ -> yield typ
    | TypeId.Const (TypeId.Pointer typ) -> yield typ
    | typ -> yield! fatalDiag $"Type \"%O{typ}\" is not an array type"
}

let rec getNumberType (num: Number) =
    match num with
    | Number.Integer _
    | Number.HexInteger _
    | Number.BinaryInteger _ -> TypeId.Int64
    | Number.Double _ -> TypeId.Double
    | Negative num -> getNumberType num

let isStructVariableWithMember (name: string) memberName (context: SourceContext ) : bool =
    match Map.tryFind name context.NameTypeEnv.Value with
    | Some variableTypeId ->
        match TypeId.unwrapPointerAndConst variableTypeId.TypeId with
        | Named typename ->
            match locateTypeDecl typename { context with CurrentSource = variableTypeId.Source } with
            | Ok (typeDecl, []) ->
                match MList.tryLookup memberName typeDecl.TypeDecl.Fields with
                | Some _ -> true
                | None -> false
            | Ok _
            | Error _ -> false
        | _ -> false
    | None -> false

let rec getExpressionType (expr: Expression) : TypeCheckerM.M<SourceContext, TypeRef> = tchecker {
    let! context = context
    let! source = getFromContext (fun c -> c.CurrentSource)

    match expr with
    | Constant (Value.String _) -> yield { TypeId = TypeId.Const (TypeId.Pointer (TypeId.Const TypeId.Char)); Source = source }
    | Constant (Value.Number num) -> yield { TypeId = getNumberType num; Source = source }
    | Constant (Value.Boolean _) -> yield { TypeId = TypeId.Bool; Source = source }
    | Constant (Value.Char _) -> yield { TypeId = TypeId.Char; Source = source }

    | Variable name ->
        match Map.tryFind name context.NameTypeEnv.Value with
        | Some typ -> yield typ
        | None -> yield! fatalDiag $"Can't find variable named \"%s{name}\""

    | FuncCall (name, args) ->
        let! func = locateFunctionDecl name
        let expectedArgsCount = List.length func.Function.Args
        let actualArgsCount = List.length args

        if expectedArgsCount <> actualArgsCount then yield! diag $"Function requires %d{expectedArgsCount} arguments, but actually supplied %d{actualArgsCount}"
        else
            let funcArgs = func.Function.Args |> List.map (fun (name, typ) -> (name, { TypeRef.TypeId = typ; Source = func.Source }))

            for (index, (arguName, arguType)), actualExpr in List.zip (List.indexed funcArgs) args do
                let! actualType = getExpressionType actualExpr
                if not (TypeId.isTypesEqual arguType actualType) then
                    yield! diag $"Function accepts \"%O{arguType}\" as %d{index}th(%s{arguName}) argument, but actual %d{index}th argument is of \"%O{actualType}\" type"

        yield { TypeId = func.Function.ReturnType; Source = func.Source }

    | MemberAccess (Variable name, memberName)
        when (isStructVariableWithMember name memberName context) ->
        let variableTypeId = Map.find name context.NameTypeEnv.Value
        match TypeId.unwrapPointerAndConst variableTypeId.TypeId with
        | Named typename ->
            let! typeDecl = checkWithContext' (fun c -> { c with CurrentSource = variableTypeId.Source }) (locateTypeDecl typename)
            match MList.tryLookup memberName typeDecl.TypeDecl.Fields with
            | Some fieldType -> yield { TypeId = fieldType; Source = typeDecl.Source }
            | None -> yield failwithf "Should not happen as case covered by isStructVariableWithMember"
        | _ -> yield failwithf "Should not happen as case covered by isStructVariableWithMember"

    | MemberAccess (Variable alias, name) when getAliasedSource alias context |> Result.isOk ->
        let! varDecl = locateVariableDecl { Name = name; Alias = Some alias }
        yield { TypeId = varDecl.Variable.TypeId; Source = varDecl.Source }

    | MemberAccess (left, field) ->
        let! typ = getExpressionType left
        match TypeId.unwrapPointerAndConst typ.TypeId with
        | Named name ->
            let! typeDecl = locateTypeDecl name
            match MList.tryLookup field typeDecl.TypeDecl.Fields with
            | Some typE -> yield { TypeId = typE; Source = typeDecl.Source }
            | None -> yield! fatalDiag $"Type \"%O{typeDecl}\" doesn't have field \"%s{field}\""
        | _ -> yield! fatalDiag $"Type \"%O{typ}\" is not struct/union and thus doesn't have field \"%s{field}\""

    | BinaryExpression binaryExpr ->
        let! leftType  = getExpressionType binaryExpr.Left
        let! rightType = getExpressionType binaryExpr.Right
        let leftTyp = leftType.TypeId
        let rightTyp = rightType.TypeId

        match binaryExpr.Kind with
        | BinaryExpressionKind.Plus
        | BinaryExpressionKind.Minus ->

            (* If (left is pointer AND right is numeric) OR (left is numeric AND right is pointer) it means
               we are dealing with pointer arithmetics *)
            if (TypeId.isPointerType leftTyp && TypeId.isUnsigned rightTyp) then yield leftType
            elif (TypeId.isPointerType rightTyp && TypeId.isUnsigned leftTyp) then yield rightType
            else

                if not (TypeId.isTypesEqual leftType rightType) then
                    yield! diag $"Arguments for operators '+', '-' has to be the same (%O{leftTyp}, %O{rightTyp})"

                if not (TypeId.isNumericType leftTyp) then
                    yield! diag "Operators '+', '-' accept only numeric & pointer types"

                if TypeId.isUnsigned leftTyp then yield { leftType with TypeId = TypeId.Uint64 }
                elif TypeId.isSigned leftTyp then yield { leftType with TypeId = TypeId.Int64 }
                elif TypeId.isFloat leftTyp then yield { leftType with TypeId = TypeId.Double }
                else yield! fatalDiag $"Can't determine result type of '%O{leftType} (+ -) '%O{rightType}' operation."

        | BinaryExpressionKind.Multiply
        | BinaryExpressionKind.Division ->

            if not (TypeId.isTypesEqual leftType rightType) then
                yield! diag $"Arguments for operators '*', '/' has to be the same (%O{leftTyp}, %O{rightTyp})"

            if TypeId.isUnsigned leftTyp then yield { leftType with TypeId = TypeId.Uint64 }
            elif TypeId.isSigned leftTyp then yield { leftType with TypeId = TypeId.Int64 }
            elif TypeId.isFloat leftTyp then yield { leftType with TypeId = TypeId.Double }
            else yield! fatalDiag $"Can't determine result type of '%O{leftType} (* /) %O{rightType}' operation."

        | BinaryExpressionKind.Equal
        | BinaryExpressionKind.NotEqual
        | BinaryExpressionKind.Less
        | BinaryExpressionKind.LessOrEqual
        | BinaryExpressionKind.Greater
        | BinaryExpressionKind.GreaterOrEqual ->
            if not (TypeId.isBool leftTyp) then yield! diag $"Left boolean comparison argument must be of type bool, not \%O{leftTyp}"
            if not (TypeId.isBool rightTyp) then yield! diag $"Right boolean comparison argument must be of type bool, not \%O{rightTyp}"

            yield { TypeId = TypeId.Bool; Source = source }

        | BinaryExpressionKind.Or
        | BinaryExpressionKind.And ->

            if not (TypeId.isTypesEqual leftType rightType) then
                yield! diag $"Arguments for operators 'or', 'and' has to be the same (%O{leftTyp}, %O{rightTyp})"

            if TypeId.isUnsigned leftTyp then yield { leftType with TypeId = TypeId.Uint64 }
            elif TypeId.isSigned leftTyp then yield { leftType with TypeId = TypeId.Int64 }
            elif TypeId.isBool leftTyp then yield { leftType with TypeId = TypeId.Bool }
            else yield! fatalDiag $"Can't determine result type of '%O{leftTyp} (or and) '%O{rightTyp}' operation."

        | BinaryExpressionKind.Xor
        | BinaryExpressionKind.RShift
        | BinaryExpressionKind.LShift ->

            if not (TypeId.isTypesEqual leftType rightType) then
                yield! diag $"Arguments for operators 'xor', '>>', '<<' has to be the same (%O{leftTyp}, %O{rightTyp})"

            if TypeId.isUnsigned leftTyp then yield { leftType with TypeId = TypeId.Uint64 }
            elif TypeId.isSigned rightTyp then yield { leftType with TypeId = TypeId.Int64 }
            else yield! fatalDiag $"Can't determine result type of '%O{leftTyp} (>> <<) %O{rightTyp}' operation."

    | ArrayAccess (array, indexExpr) ->
        let! arrayType = getExpressionType array
        let! indexType = getExpressionType indexExpr
        let! arraySubitemType = getArraySubitemType arrayType.TypeId

        if TypeId.unwrapConst indexType.TypeId <> TypeId.Int64 then yield! diag $"Array's index type must be int64, not \"%O{indexType}\""
        yield { TypeId = arraySubitemType; Source = arrayType.Source }

    | StructCreation (name, fieldsInits) ->
        let! typeType = locateTypeDecl name

        let fieldDuplicates = MList.duplicates (List.map fst fieldsInits)
        if not (List.isEmpty fieldDuplicates) then
            yield! diag $"%O{typeType.TypeDecl.TypeType}'s initializer has duplicated fields: %A{fieldDuplicates}"

        if typeType.TypeDecl.TypeType = TypeType.Union &&
           List.length fieldsInits > 1 then
               yield! diag $"Union \"%s{typeType.TypeDecl.Name}\"'s initializer must initialize only one (1) field at the time."

        for field, expr in fieldsInits do
            let! rightType = getExpressionType expr
            match MList.tryLookup field typeType.TypeDecl.Fields with
            | Some actualFieldType ->
                let leftType = { TypeId = actualFieldType; Source = typeType.Source }
                if not (TypeId.isTypesEqual leftType rightType) then yield! diag $"Field \"%s{field}\" of \"%O{name}\" has type \"%O{leftType}\" but init expression has type \"%O{rightType}\""
            | None -> yield! diag $"Type \"%O{name}\" doesn't have field \"%s{field}\""

        yield { TypeId = TypeId.Named name; Source = typeType.Source }

    | Bininversion expr ->
        let! typ = getExpressionType expr
        let typE = TypeId.unwrapConst typ.TypeId
        if not (List.contains typE TypeId.integerTypes) then yield! diag $"Operator '~' only accepts numeric types as argument, but not \"%O{typ}\""
        yield typ
}

let rec checkFunctionStatement (statement: Statement) : TypeCheckerM.M<SourceFunctionContext, unit> = tchecker {
    let! source = getFromContext (fun c -> c.CurrentSource)
    let! sourceContext = getFromContext (fun c -> c.WithSource ())

    match statement with
    | Statement.VarDecl (name, typ, expr) ->
        do! checkWithContext sourceContext (checkNamedTypeIdExists typ)

        if TypeId.isVoid typ then
            yield! diag' $"Function local variable \"%s{name}\" can't have type 'void'."

        match expr with
        | Some expr ->
            let! exprTyp = checkWithContext sourceContext (getExpressionType expr)
            let variableType = { TypeId = typ; Source = source }
            if not (TypeId.isTypesEqual variableType exprTyp) then yield! diag' $"Variables's \"%s{name}\" (\"%O{variableType}\") init expression has different type \"%O{exprTyp}\""
        | None -> ()

    | Statement.VarAssignment (left, right) ->
        let! leftType = checkWithContext sourceContext (getExpressionType left)
        let! rightType = checkWithContext sourceContext (getExpressionType right)
        if not (TypeId.isTypesEqual leftType rightType) then yield! diag' $"Left(target) expression of assignment has type \"%O{leftType}\", but right part has \"%O{rightType}\""
        if TypeId.isConst leftType.TypeId then yield! diag' $"You can't assign to a constant variable (\"%O{leftType}\")"
        match left with
        | Variable _
        | MemberAccess _
        | ArrayAccess _ -> ()
        | _ -> yield! diag' "You can only assign to variable, member access, array access expressions."

    | Statement.If (main, elseIfs, elsE) ->
        let checkIfCond cond : TypeCheckerM.M<SourceFunctionContext, unit> = tchecker {
            let! exprType = checkWithContext sourceContext (getExpressionType cond.Condition)
            if not (TypeId.isBool exprType.TypeId) then yield! diag' $"'if' condition must be of type bool, but fond \"%O{exprType}\""
            do! checkStatementList cond.Body
        }
        do! checkIfCond main
        for cond in elseIfs do
            do! checkIfCond cond
        do! checkStatementList elsE

    | Statement.For (name, startExpr, endExpr, stepExpr, body) ->
        let! startType = checkWithContext sourceContext (getExpressionType startExpr)
        let! endType = checkWithContext sourceContext (getExpressionType endExpr)

        if startType.TypeId <> TypeId.Int64 then yield! diag' $"For loop's start value must be of type int64, but found \"%O{startType}\""
        if endType.TypeId <> TypeId.Int64 then yield! diag' $"For loop's end value must be of type int64, but found \"%O{endType}\""

        match stepExpr with
        | Some expr ->
            let! typ = checkWithContext sourceContext (getExpressionType expr)
            if typ.TypeId <> TypeId.Int64 then yield! diag' $"For loop's step value must be of type int64, but found \"%O{typ}\""
        | None -> ()

        let! context = getFromContext (fun c -> { c with NameTypeEnv = lazy (Map.add name { TypeId = TypeId.Int64; Source = source } c.NameTypeEnv.Value) })
        do! checkWithContext context (checkStatementList body)

    | Statement.While (condition, body) ->
        let! exprType = checkWithContext sourceContext (getExpressionType condition)
        if not (TypeId.isBool exprType.TypeId) then yield! diag' $"While's condition must be of type bool, but found \"%O{exprType}\""
        do! checkStatementList body

    | Statement.Defer body ->
        do! checkStatementList body

    | Statement.Return ->
        let! func = getFromContext (fun c -> c.CurrentFunction)
        if func.ReturnType <> TypeId.Void then yield! diag' $"Function return type must be void in order to return nothing, but current return type is \"%O{func.ReturnType}\""

    | Statement.ReturnExpr expr ->
        let! typ = checkWithContext sourceContext (getExpressionType expr)
        let! func = getFromContext (fun c -> c.CurrentFunction)
        if not (TypeId.isTypesEqual typ { TypeId = func.ReturnType; Source = sourceContext.CurrentSource }) then yield! diag' $"Function's return type is \"%O{func.ReturnType}\" but return statement's type is \"%O{typ}\""

    | Statement.Expression expr ->
        let! _ = checkWithContext sourceContext (getExpressionType expr)
        match expr with
        | FuncCall _ -> ()
        | _ -> yield! diag' "Only function call expression is allowed as statement."

}

and checkStatementList (stats: Statement list) : TypeCheckerM.M<SourceFunctionContext, unit> = tchecker {
    let! context = context
    let folder (xs, context) statement =
        let xs = xs @ [ fun () -> checkWithContext context (checkFunctionStatement statement) ]
        let context =
            match statement with
            | VarDecl (name, typeId, _) -> { context with NameTypeEnv = lazy (Map.add name { TypeId = typeId; Source = context.CurrentSource } context.NameTypeEnv.Value) }
            | _ -> context
        (xs, context)

    let checks, _ = stats |> List.fold folder ([], context)
    for check in checks do
        do! check ()
}

let checkVariableDeclaration (variable: Variable) : TypeCheckerM.M<SourceContext, unit> = tchecker {
    let! source = getFromContext (fun c -> c.CurrentSource)
    do! checkNamedTypeIdExists variable.TypeId

    if TypeId.isVoid variable.TypeId then
        yield! diag $"Variable \"%s{variable.Name}\" can't have void type."

    if Option.exists (fun m -> m = Modifier.Extern) variable.Modifier &&
       Option.isSome variable.InitExpr then
           yield! diag $"Variable \"%s{variable.Name}\" is extern and has init expression."

    match variable.InitExpr with
    | Some expr ->
        let! exprType = getExpressionType expr
        if not (TypeId.isTypesEqual { TypeId = variable.TypeId; Source = source } exprType) then yield! diag $"Variable \"%s{variable.Name}\" has type \"%O{variable.TypeId}\", but init expr has type \"%O{exprType}\""
        if not (isExpressionConstant expr) then yield! diag $"Variable's \"%s{variable.Name}\" init expression must be constant."
    | None -> ()
}

let checkFunctionDeclaration (func: Function) : TypeCheckerM.M<SourceContext, unit> = tchecker {
    for typ in func.Args |> List.map snd do
        do! checkNamedTypeIdExists typ

    let duplicatedArgs = func.Args |> List.map fst |> MList.duplicates
    if not (List.isEmpty duplicatedArgs) then yield! diag $"Function \"%s{func.Name}\" contains duplicated arguments: %A{duplicatedArgs}"

    let voidFields = func.Args |> List.filter (fun (_, t) -> TypeId.isVoid t)
    if not (List.isEmpty voidFields) then yield! diag $"Function \"%s{func.Name}\" has void arguments: %A{List.map fst voidFields}"

    do! checkNamedTypeIdExists func.ReturnType

    if Option.exists (fun m -> m = Modifier.Extern) func.Modifier &&
       not (List.isEmpty func.Body) then
           yield! diag $"Function \"%s{func.Name}\" is extern function and have a body."

    let! source = getFromContext (fun c -> c.CurrentSource)
    let! context = getFromContext (fun c -> c.WithFunction func)
    let env = lazy (
            func.Args
            |> List.fold (fun env (field, typ) -> Map.add field { TypeId = typ; Source = source } env) context.NameTypeEnv.Value
        )
    let context = { context with NameTypeEnv = env }

    do! checkWithContext context (checkStatementList func.Body)
}

let checkTypeDeclaration (typ: TypeDecl) : TypeCheckerM.M<SourceContext, unit> = tchecker {
    let duplicatedFields = typ.Fields |> List.map fst |> MList.duplicates
    if not (List.isEmpty duplicatedFields) then yield! diag $"Type \"%s{typ.Name}\" has duplicated fields: %A{duplicatedFields}"

    for typ in typ.Fields |> List.map snd do
        do! checkNamedTypeIdExists typ

    let voidFields = typ.Fields |> List.map snd |> List.filter TypeId.isVoid
    if not (List.isEmpty voidFields) then yield! diag $"Type \"%s{typ.Name}\" has fields with 'void' type."

    let visitedDecls = System.Collections.Generic.HashSet<TypeDecl> ()

    (* Check that type declaration doesn't have itself as a field somewhere deep in hierarchy *)
    let rec visitFields (typRef: TypeDeclRef) = tchecker {
        for typ in typRef.TypeDecl.Fields |> List.map snd do
            match TypeId.unwrapConst typ with
            | Named name ->
                let! typ = checkWithContext' (fun c -> { c with SourceContext.CurrentSource = typRef.Source}) (locateTypeDecl name)
                if not (visitedDecls.Contains typ.TypeDecl) then
                    (visitedDecls.Add typ.TypeDecl) |> ignore
                    do! visitFields typ
            | _ -> ()
    }

    let! source = getFromContext (fun c -> c.CurrentSource)
    do! visitFields { TypeDecl = typ; Source = source }

    if visitedDecls.Contains typ then
            yield! diag $"Type \"%s{typ.Name}\" has circular reference to itself within some of it's fields (eg, 'struct Foo {{ foo: Foo }}')"
}

let checkSourceDeclarations () : TypeCheckerM.M<SourceContext, unit>  = tchecker {
    let! decls = getFromContext (fun c -> c.CurrentSource.Declarations)
    for decl in decls do
        match decl with
        | Declaration.Variable var -> yield! checkVariableDeclaration var
        | Declaration.Function func -> yield! checkFunctionDeclaration func
        | Declaration.Type typ -> yield! checkTypeDeclaration typ
}

let rec getTypeIdSize (typ: TypeRef) : TypeCheckerM.M<SourceContext, int<bytesize>> = tchecker {
    match TypeId.unwrapConst typ.TypeId with
    | TypeId.Int8 | TypeId.Uint8 -> yield 1<bytesize>
    | TypeId.Int16 | TypeId.Uint16 -> yield 2<bytesize>
    | TypeId.Int32 | TypeId.Uint32 -> yield 4<bytesize>
    | TypeId.Int64 | TypeId.Uint64 -> yield 8<bytesize>
    | TypeId.Float -> yield 4<bytesize>
    | TypeId.Double -> yield 8<bytesize>
    | TypeId.Bool -> yield 8<bytesize>
    | TypeId.Char -> yield 1<bytesize>
    | TypeId.Void -> yield 0<bytesize>
    | TypeId.Pointer _ -> yield 8<bytesize>
    | TypeId.Named name ->
        let! typeDecl = checkWithContext' (fun c -> { c with CurrentSource = typ.Source }) (locateTypeDecl name)
        let! fieldSizes =
            typeDecl.TypeDecl.Fields
            |> List.map snd
            |> List.map (fun t -> getTypeIdSize { TypeId = t; Source = typeDecl.Source })
            |> unwrapList

        match typeDecl.TypeDecl.TypeType with
        | TypeType.Struct -> yield fieldSizes |> List.sumBy (fun s -> align s 8) |> int |> LanguagePrimitives.Int32WithMeasure
        | TypeType.Union -> yield align (List.max fieldSizes ) 8 |> int |> LanguagePrimitives.Int32WithMeasure
    | typ -> yield failwithf $"This type id should have been covered: %O{typ}"
}

let getExpressionSize expression = tchecker {
    let! exprType = getExpressionType expression
    let! size = getTypeIdSize exprType
    yield size
}

let makeContext (source: Source) (program: Program) =
    let ctx = { CurrentSource = source
                Program = program
                NameTypeEnv= lazy Map.empty  }

    let folder env source =
        source
        |> getVariableDeclarations
        |> List.fold (fun env variable -> (Map.add variable.Name { TypeId = variable.TypeId; Source = source } env)) env

    match getImplicitlyReferencedSources ctx with
    | Ok (sources, [])->
        let env = lazy ( sources @ [ source ] |> List.fold folder Map.empty )
        Ok { ctx with NameTypeEnv = env }
    | Ok (_, errors)
    | Error errors -> Error errors

let typeCheckProgram (program: Program) =
    let checkSource (source: Source) =
            match makeContext source program with
            | Ok context ->
                let tChecker = tchecker {
                    do! checkOpenDirectives ()
                    do! checkUniqueDeclarations ()
                    do! checkSourceDeclarations ()
                }
                tChecker context
            | Error errors -> Error errors

    let checks =
        program.Sources
        |> List.map checkSource
        |> List.fold (fun diags result ->
            match result with
            | Ok ((), diags2) -> diags @ diags2
            | Error diags2 -> diags @ diags2) []

    checks
