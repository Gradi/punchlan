module LibPunchLan.Comp

open LibPunchLan.Parsing

let getTypeDeclarations (source: Source) =
    source.Declarations
    |> List.map (fun decl ->
        match decl with
        | Declaration.Function _ -> None
        | Declaration.Variable _ -> None
        | Declaration.Type typ -> Some typ)
    |> List.choose id

let getVariableDeclarations (source: Source) =
    source.Declarations
    |> List.map (fun decl ->
        match decl with
        | Declaration.Variable var -> Some var
        | Declaration.Function _ -> None
        | Declaration.Type _ -> None)
    |> List.choose id

let getFunctionDeclarations (source: Source) =
    source.Declarations
    |> List.map (fun decl ->
        match decl with
        | Declaration.Function func -> Some func
        | Declaration.Variable _ -> None
        | Declaration.Type _ -> None)
    |> List.choose id

let rec unwrapPointerAndConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapPointerAndConst typ
    | Pointer typ -> unwrapPointerAndConst typ
    | typ -> typ

let rec unwrapConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapConst typ
    | typ -> typ

let isConstType (typ: TypeId) =
    match typ with
    | Const _ -> true
    | _ -> false

let isPointerType (typ: TypeId) =
    match typ with
    | TypeId.Pointer _
    | Const (TypeId.Pointer _) -> true
    | _ -> false

let rec isExpressionConstant (expr: Expression) =
    match expr with
    | Constant _ -> true
    | StructCreation (_, fields) -> fields |> List.map snd |> List.forall isExpressionConstant
    | _ -> false
