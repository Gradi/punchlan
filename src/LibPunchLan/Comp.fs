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

let align value mul =
    match value % mul with
    | 0 -> value
    | rem -> value + (mul - rem)
