module LibPunchLan.Comp

open LibPunchLan.Parsing

(* Unaligned size in bytes *)
[<Measure>] type bytesize

(* Aligned size in bytes *)
[<Measure>] type albytesize

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

let align (value: int<bytesize>) (mul: int) : int<albytesize> =
    let value = (int value)
    match value % mul with
    | 0 -> LanguagePrimitives.Int32WithMeasure value
    | rem -> LanguagePrimitives.Int32WithMeasure (value + (mul - rem))
