module LibPunchLan.TypeChecking.TypeChecker

open LibPunchLan
open LibPunchLan.TypeChecking.TypeCheckerM
open LibPunchLan.Comp
open LibPunchLan.Parsing
open LibPunchLan.Lexing

let integerTypes =
    let t = [ TypeId.Int8; TypeId.Uint8; TypeId.Int16; TypeId.Uint16; TypeId.Int32; TypeId.Uint32
              TypeId.Int64; TypeId.Uint64 ]
    t
    |> List.append (t |> List.map (fun t -> TypeId.Const t))

let numericTypes =
    let t = [ TypeId.Int8; TypeId.Uint8; TypeId.Int16; TypeId.Uint16; TypeId.Int32; TypeId.Uint32
              TypeId.Int64; TypeId.Uint64; TypeId.Float; TypeId.Double ]
    t
    |> List.append (t |> List.map (fun t -> TypeId.Const t))

let checkOpenDirectives () = tchecker {
    let! odis = getFromContext (fun c -> c.CurrentSource.OpenDirectives)

    let paths = odis |> List.map (fun od -> od.Path) |> MList.duplicates
    let aliases = odis |> List.map (fun od -> od.Alias) |> List.choose id |> MList.duplicates

    if not (List.isEmpty paths) then yield! diag $"Source contains duplicated open paths: %A{paths}"
    if not (List.isEmpty aliases) then yield! diag $"Source contains duplicated open aliases: %A{aliases}"
}

let checkUniqueDeclarations () = tchecker {
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

let checkNamedTypeIdExists (typ: TypeId) = tchecker {
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

let getArraySubitemType (typ: TypeId) = tchecker {
    match typ with
    | TypeId.Pointer typ -> yield typ
    | TypeId.Const (TypeId.Pointer typ) -> yield typ
    | typ -> yield! fatalDiag $"Type \"%O{typ}\" is not an array type"
}

let rec getNumberType (num: Number) =
    match num with
    | Number.Integer _
    | Number.HexInteger _
    | Number.BinaryInteger _ -> TypeId.Const TypeId.Int64
    | Number.Double _ -> TypeId.Const TypeId.Double
    | Negative num -> getNumberType num

let rec getExpressionType (expr: Expression) = tchecker {
    let! context = context
    match expr with
    | Constant (Value.String _) -> yield (TypeId.Pointer (TypeId.Const TypeId.Char))
    | Constant (Value.Number num) -> yield getNumberType num
    | Constant (Value.Boolean _) -> yield TypeId.Const TypeId.Bool
    | Constant (Value.Char _) -> yield TypeId.Const TypeId.Char

    | Variable name ->
        match Map.tryFind name context.NameTypeEnv.Value with
        | Some typ -> yield typ
        | None -> yield! fatalDiag $"Can't find variable named \"%s{name}\""

    | FuncCall (name, args) ->
        let! func = locateFunctionDecl name
        let expectedArgs = List.length func.Args
        let actualArgs = List.length args

        if expectedArgs <> actualArgs then yield! diag $"Function requires %d{expectedArgs} arguments, but actually supplied %d{actualArgs}"
        else
            for (index, (arguName, arguType)), actualExpr in List.zip (List.indexed func.Args) args do
                let! actualType = getExpressionType actualExpr
                if not (TypeId.isTypesEqual arguType actualType) then
                    yield! diag $"Function accepts \"%O{arguType}\" as %d{index}th(%s{arguName}) argument, but actual %d{index}th argument is of \"%O{actualType}\" type"

        yield func.ReturnType

    | MemberAccess (left, field) ->
        let! typ = getExpressionType left
        match TypeId.unwrapPointerAndConst typ with
        | Named name ->
            let! typeDecl = locateTypeDecl name
            match MList.tryLookup field typeDecl.Fields with
            | Some typ -> yield typ
            | None -> yield! fatalDiag $"Type \"%s{typeDecl.Name}\" doesn't have field \"%s{field}\""
        | _ -> yield! fatalDiag $"Type \"%O{typ}\" is not struct/union and thus doesn't have field \"%s{field}\""

    | BinaryExpression (BinaryExpression.Plus (left, right))
    | BinaryExpression (BinaryExpression.Minus (left, right)) ->
        let! leftTyp = getExpressionType left
        let! rightTyp = getExpressionType right
        let leftTyp = TypeId.unwrapConst leftTyp
        let rightTyp = TypeId.unwrapConst rightTyp
        if leftTyp <> rightTyp then yield! diag $"Arguments for operators '+', '-' has to be the same (%O{leftTyp}, %O{rightTyp})"

        if List.contains leftTyp numericTypes then yield ()
        elif TypeId.isPointerType leftTyp then yield ()
        else
            yield! diag "Operators '+', '-' accept only numeric & pointer types"

        yield leftTyp

    | BinaryExpression (BinaryExpression.Multiply (left, right))
    | BinaryExpression (BinaryExpression.Division (left, right)) ->
        let! leftTyp = getExpressionType left
        let! rightTyp = getExpressionType right
        let leftTyp = TypeId.unwrapConst leftTyp
        let rightTyp = TypeId.unwrapConst rightTyp

        if leftTyp <> rightTyp then yield! diag $"Arguments for operators '*', '/' has to be the same (%O{leftTyp}, %O{rightTyp})"
        yield leftTyp

    | BinaryExpression (BinaryExpression.Equal (left, right))
    | BinaryExpression (BinaryExpression.NotEqual (left, right))
    | BinaryExpression (BinaryExpression.Less (left, right))
    | BinaryExpression (BinaryExpression.LessOrEqual (left, right))
    | BinaryExpression (BinaryExpression.Greater (left, right))
    | BinaryExpression (BinaryExpression.GreaterOrEqual (left, right)) ->
        let! leftTyp = getExpressionType left
        let! rightTyp = getExpressionType right
        if TypeId.unwrapConst leftTyp <> TypeId.Bool then yield! diag $"Left boolean comparison argument must be of type bool, not \%O{leftTyp}"
        if TypeId.unwrapConst rightTyp <> TypeId.Bool then yield! diag $"Right boolean comparison argument must be of type bool, not \%O{rightTyp}"

        yield TypeId.Bool

    | BinaryExpression (BinaryExpression.Or (left, right))
    | BinaryExpression (BinaryExpression.And (left, right)) ->
        let! leftTyp = getExpressionType left
        let! rightTyp = getExpressionType right

        if leftTyp <> rightTyp then yield! diag $"Arguments for operators 'or', 'and' has to be the same (%O{leftTyp}, %O{rightTyp})"

        if List.contains leftTyp integerTypes then yield TypeId.unwrapConst leftTyp
        elif TypeId.unwrapConst leftTyp = TypeId.Bool then yield TypeId.Bool
        else yield! fatalDiag $"Arguments for operators 'or', 'and' can be numeric or boolean, but found \"%O{leftTyp}\""

    | BinaryExpression (BinaryExpression.Xor (left, right))
    | BinaryExpression (BinaryExpression.RShift (left, right))
    | BinaryExpression (BinaryExpression.LShift (left, right)) ->
        let! leftTyp = getExpressionType left
        let! rightTyp = getExpressionType right
        let leftTyp = TypeId.unwrapConst leftTyp
        let rightTyp = TypeId.unwrapConst rightTyp
        if leftTyp <> rightTyp then yield! diag $"Arguments for operators 'xor', '>>', '<<' has to be the same (%O{leftTyp}, %O{rightTyp})"

        if not (List.contains leftTyp numericTypes ) then yield! fatalDiag $"Arguments for operators 'xor, '>>', '<<' has to be of numeric type, but found \"%O{leftTyp}\""
        yield leftTyp

    | ArrayAccess (array, indexExpr) ->
        let! arrayType = getExpressionType array
        let! indexType = getExpressionType indexExpr
        let! arraySubitemType = getArraySubitemType arrayType

        if TypeId.unwrapConst indexType <> TypeId.Int64 then yield! diag $"Array's index type must be int64, not \"%O{indexType}\""
        yield arraySubitemType

    | StructCreation (name, fieldsInits) ->
        let! typeType = locateTypeDecl name

        for field, expr in fieldsInits do
            let! typ = getExpressionType expr
            match MList.tryLookup field typeType.Fields with
            | Some actualFieldType ->
                if typ <> actualFieldType then yield! diag $"Field \"%s{field}\" of \"%O{name}\" has type \"%O{actualFieldType}\" but init expression has type \"%O{typ}\""
            | None -> yield! diag $"Type \"%O{name}\" doesn't have field \"%s{field}\""

        yield TypeId.Named name

    | Bininversion expr ->
        let! typ = getExpressionType expr
        let typ = TypeId.unwrapConst typ
        if not (List.contains typ integerTypes) then yield! diag $"Operator '~' only accepts numeric types as argument, but not \"%O{typ}\""
        yield typ
}

let rec checkFunctionStatement (statement: Statement) = tchecker {
    match statement with
    | VarDecl (name, typ, expr) ->
        do! checkNamedTypeIdExists typ
        match expr with
        | Some expr ->
            let! exprTyp = getExpressionType expr
            if TypeId.unwrapConst exprTyp <> TypeId.unwrapConst typ then yield! diag $"Variables's \"%s{name}\" (\"%O{typ}\") init expression has different type \"%O{exprTyp}\""
        | None -> ()

    | VarAssignment (left, right) ->
        let! leftType = getExpressionType left
        let! rightType = getExpressionType right
        if leftType <> rightType then yield! diag $"Left(target) expression of assignment has type \"%O{leftType}\", but right part has \"%O{rightType}\""
        if TypeId.isConstType leftType then yield! diag $"You can't assign to a constant variable (\"%O{leftType}\")"
        match left with
        | Variable _
        | MemberAccess _
        | ArrayAccess _ -> ()
        | _ -> yield! diag "You can only assign to variable, member access, array access expressions."

    | Statement.If (main, elseIfs, elsE) ->
        let checkIfCond cond = tchecker {
            let! exprType = getExpressionType cond.Condition
            if exprType <> TypeId.Bool then yield! diag $"'if' condition must be of type bool, but fond \"%O{exprType}\""
            do! checkStatementList cond.Body
        }
        do! checkIfCond main
        for cond in elseIfs do
            do! checkIfCond cond
        do! checkStatementList elsE

    | Statement.For (name, startExpr, endExpr, stepExpr, body) ->
        let! startType = getExpressionType startExpr
        let! endType = getExpressionType endExpr

        if startType <> TypeId.Int64 then yield! diag $"For loop's start value must be of type int64, but found \"%O{startType}\""
        if endType <> TypeId.Int64 then yield! diag $"For loop's end value must be of type int64, but found \"%O{endType}\""

        match stepExpr with
        | Some expr ->
            let! typ = getExpressionType expr
            if typ <> TypeId.Int64 then yield! diag $"For loop's step value must be of type int64, but found \"%O{typ}\""
        | None -> ()

        let! context = getFromContext (fun c -> { c with NameTypeEnv = lazy (Map.add name TypeId.Int64 c.NameTypeEnv.Value) })
        do! checkWithContext context (checkStatementList body)

    | Statement.While (condition, body) ->
        let! exprType = getExpressionType condition
        if exprType <> TypeId.Bool then yield! diag $"While's condition must be of type bool, but found \"%O{exprType}\""
        do! checkStatementList body

    | Statement.Defer body ->
        do! checkStatementList body

    | Statement.Return ->
        let! func = getFuncFromContext
        if func.ReturnType <> TypeId.Void then yield! diag $"Function return type must be void in order to return nothing, but current return type is \"%O{func.ReturnType}\""

    | ReturnExpr expr ->
        let! typ = getExpressionType expr
        let! func = getFuncFromContext
        if typ <> func.ReturnType then yield! diag $"Function's return type is \"%O{func.ReturnType}\" but return statement's type is \"%O{typ}\""

    | Statement.Expression expr ->
        let! _ = getExpressionType expr
        match expr with
        | FuncCall _ -> ()
        | _ -> yield! diag "Only function call expression is allowed as statement."

}

and checkStatementList (stats: Statement list) = tchecker {
    let folder (xs, context) statement =
        let xs = xs @ [ fun () -> checkWithContext context (checkFunctionStatement statement) ]
        let context =
            match statement with
            | VarDecl (name, typeId, _) -> { context with NameTypeEnv = lazy (Map.add name typeId context.NameTypeEnv.Value) }
            | _ -> context
        (xs, context)

    let! context = context
    let checks, _ = stats |> List.fold folder ([], context)
    for check in checks do
        do! check ()
}

let checkVariableDeclaration (variable: Variable) = tchecker {
    do! checkNamedTypeIdExists variable.TypeId

    match variable.InitExpr with
    | Some expr ->
        let! exprType = getExpressionType expr
        if exprType <> variable.TypeId then yield! diag $"Variable \"%s{variable.Name}\" has type \"%O{variable.TypeId}\", but init expr has type \"%O{exprType}\""
        if not (isExpressionConstant expr) then yield! diag $"Variable's \"%s{variable.Name}\" init expression must be constant."
    | None -> ()
}

let checkFunctionDeclaration (func: Function) = tchecker {
    for typ in func.Args |> List.map snd do
        do! checkNamedTypeIdExists typ

    let duplicatedArgs = func.Args |> List.map fst |> MList.duplicates
    if not (List.isEmpty duplicatedArgs) then yield! diag $"Function \"%s{func.Name}\" contains duplicated arguments: %A{duplicatedArgs}"

    do! checkNamedTypeIdExists func.ReturnType

    let! context = getFromContext (fun c -> { c with CurrentFunction = Some func })
    let env =
        func.Args
        |> List.fold (fun env (field, typ) -> lazy (Map.add field typ env.Value)) context.NameTypeEnv
    let context = { context with NameTypeEnv = env }

    do! checkWithContext context (checkStatementList func.Body)
}

let checkTypeDeclaration (typ: TypeDecl) = tchecker {
    let duplicatedFields = typ.Fields |> List.map fst |> MList.duplicates
    if not (List.isEmpty duplicatedFields) then yield! diag $"Type \"%s{typ.Name}\" has duplicated fields: %A{duplicatedFields}"

    for typ in typ.Fields |> List.map snd do
        do! checkNamedTypeIdExists typ

    let visitedDecls = System.Collections.Generic.HashSet<TypeDecl> ()

    (* Check that type declaration doesn't have itself as a field somewhere deep in hierarchy *)
    let rec visitFields (typ: TypeDecl) = tchecker {
        for typ in typ.Fields |> List.map snd do
            match TypeId.unwrapAllConst typ with
            | Named name ->
                let! typ = locateTypeDecl name
                if not (visitedDecls.Contains typ) then
                    (visitedDecls.Add typ) |> ignore
                    do! visitFields typ
            | _ -> ()
    }

    do! visitFields typ

    if visitedDecls.Contains typ then
            yield! diag $"Type \"%s{typ.Name}\" has circular reference to itself within some of it's fields (eg, 'struct Foo {{ foo: Foo }}')"
}

let checkSourceDeclarations () = tchecker {
    let! decls = getFromContext (fun c -> c.CurrentSource.Declarations)
    for decl in decls do
        match decl with
        | Declaration.Variable var -> yield! checkVariableDeclaration var
        | Declaration.Function func -> yield! checkFunctionDeclaration func
        | Declaration.Type typ -> yield! checkTypeDeclaration typ
}


let typeCheckProgram (program: Program) =
    let checkSource (source: Source) =
        let ctx = { CurrentSource = source
                    CurrentFunction = None
                    Program = program
                    NameTypeEnv= lazy (Map.empty)  }

        let folder env source =
            source
            |> getVariableDeclarations
            |> List.fold (fun env variable -> lazy (Map.add variable.Name variable.TypeId env.Value)) env

        match getImplicitlyReferencedSources ctx with
        | Ok (sources, xs)->
            assert List.isEmpty xs
            let env =
                sources @ [ source ]
                |> List.fold folder (lazy (Map.empty))
            let ctx = { ctx with NameTypeEnv = env }
            let tChecker = tchecker {
                do! checkOpenDirectives ()
                do! checkUniqueDeclarations ()
                do! checkSourceDeclarations ()
            }

            tChecker ctx
        | Error diags -> Error diags

    let checks =
        program.Sources
        |> List.map checkSource
        |> List.fold (fun diags result ->
            match result with
            | Ok ((), diags2) -> diags @ diags2
            | Error diags2 -> diags @ diags2) []

    checks
