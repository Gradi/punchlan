module LibPunchLan.TypeChecking.TypeCheckerM

open LibPunchLan.Parsing
open LibPunchLan.Comp

type M<'a, 'b> = 'a -> Result<'b * Diagnostics, Diagnostics>

type TypeCheckerCE () =

    member _.Bind(m: M<'a, 'b>, f: 'b -> M<'a, 'c>) = (fun context ->
        match m context with
        | Ok (value, diags) ->
            match f value context with
            | Ok (value, diags2) -> Ok (value, diags @ diags2)
            | Error diags2 -> Error (diags @ diags2)
        | Error diags -> Error diags)

    member _.Zero () = (fun _ -> Ok ((), []))

    member _.YieldFrom value = value

    member _.Yield value = (fun _ -> Ok (value, []))

    member _.Delay f = f

    member _.Run f = f ()

    member _.Combine(m: M<'a, unit>, f: unit -> M<'a, 'b>) = (fun context ->
        match m context with
        | Ok ((), diags) ->
            match f () context with
            | Ok (value, diags2) -> Ok (value, diags @ diags2)
            | Error diags2 -> Error (diags @ diags2)
        | Error diags -> Error diags)

    member _.For(items: 'a seq, f: 'a -> M<'b, unit>) = (fun context ->
        use enumerator = items.GetEnumerator ()
        let mutable result = Ok ((), [])

        while Result.isOk result && enumerator.MoveNext () do
            let current = enumerator.Current
            result <-
                match result with
                | Ok ((), diags) ->
                    match f current context with
                    | Ok ((), diags2) -> Ok ((), diags @ diags2)
                    | Error diags2 -> Error (diags @ diags2)
                | Error diags -> Error diags

        result)

let tchecker = TypeCheckerCE ()

let diag msg (context: SourceContext) =
    let msg = sprintf msg
    let dia = { Diagnostic.Source = context.CurrentSource
                Function = None
                Message = msg }
    Ok ((),  [ dia ])

let fatalDiag msg (context: SourceContext) =
    match diag msg context with
    | Ok ((), diags) -> Error diags
    | Error diags -> Error diags

let diag' msg (context: SourceFunctionContext) =
    let msg = sprintf msg
    let dia = { Diagnostic.Source = context.CurrentSource
                Function = Some context.CurrentFunction
                Message = msg }
    Ok ((), [ dia ])

let fatalDiag' msg (context: SourceFunctionContext) =
    match diag' msg context with
    | Ok ((), diags) -> Error diags
    | Error diags -> Error diags

let context ctx = Ok (ctx, [])

let getFromContext f ctx = Ok (f ctx, [])

let checkWithContext context m = (fun _ -> m context)

let checkWithContext' f m = (fun context -> m (f context))

let getAliasedSource (alias: string) : M<SourceContext, Source> = tchecker {
    if System.String.IsNullOrWhiteSpace alias then
        yield! fatalDiag "Source alias is null or empty or whitespace"

    else
        let! context = context
        let paths =
            context.CurrentSource.OpenDirectives
            |> List.filter (fun od -> Option.isSome od.Alias && od.Alias.Value = alias)
            |> List.map (fun od -> od.Path)

        match paths with
        | [] -> yield! fatalDiag $"Can't find any path for alias \"%s{alias}\""
        | [ path ] ->
            let sources = context.Program.Sources |> List.filter (fun s -> s.Filename = path)
            match sources with
            | [] -> yield! fatalDiag $"Can't find source for alias \%s{alias}\" and path \"%s{path}\""
            | [ source ] -> yield source
            | sources -> yield! fatalDiag $"Found more than 1 (%d{List.length sources}) source for alias \"%s{alias}\" and path \"%s{path}\""
        | paths -> yield! fatalDiag $"Found more than 1 (%d{List.length paths}) paths for alias \"%s{alias}\""
}

let getImplicitlyReferencedSources : M<SourceContext, Source list> = tchecker {
    let! context = context

    let sources =
        context.CurrentSource.OpenDirectives
        |> List.filter (fun od -> Option.isNone od.Alias)
        |> List.map (fun od -> od.Path)
        |> List.collect (fun path -> context.Program.Sources |> List.filter (fun source -> source.Filename = path))

    yield sources
}

let locateTypeDecl (name: DotString) : M<SourceContext, TypeDeclRef> = tchecker {
    let! types = tchecker {
        match name with
        | { Alias = Some alias } ->
            let! source = getAliasedSource alias
            yield source |> getTypeDeclarations |> List.map (fun t -> { TypeDeclRef.TypeDecl = t; Source = source })
        | { Alias = None } ->
            let! sources = getImplicitlyReferencedSources
            let! source = getFromContext (fun c -> c.CurrentSource)
            let sources = source :: (List.rev sources)
            yield sources |> List.collect (fun source -> source |> getTypeDeclarations |> List.map (fun t -> { TypeDeclRef.TypeDecl = t; Source = source }))
    }

    let types = types |> List.filter (fun tref -> tref.TypeDecl.Name = name.Name)
    match types with
    | [] -> yield! fatalDiag $"Can't find type named \"%O{name}\""
    | [ typ ] -> yield typ
    | types -> yield! fatalDiag $"Found more than 1 (%d{List.length types}) types named \"%O{name}\""
}

let locateFunctionDecl (name: DotString) : M<SourceContext, FuncRef> = tchecker {
    let! funcs = tchecker {
        match name with
        | { Alias = Some alias } ->
            let! source = getAliasedSource alias
            yield source |> getFunctionDeclarations |> List.map (fun f -> { FuncRef.Function = f; Source = source })
        | { Alias = None } ->
            let! source = getFromContext (fun c -> c.CurrentSource)
            let! sources = getImplicitlyReferencedSources
            let sources = source :: (List.rev sources)
            yield sources |> List.collect (fun source -> getFunctionDeclarations source |> List.map (fun f -> { FuncRef.Function = f; Source = source }))
    }

    match funcs |> List.filter (fun fref -> fref.Function.Name = name.Name) with
    | [] -> yield! fatalDiag $"Can't find function named \"%O{name}\""
    | [ f ] -> yield f
    | fs -> yield! fatalDiag $"Found more than 1 (%d{List.length fs}) functions named \"%O{name}\""
}

let locateVariableDecl (name: DotString) : M<SourceContext, VarRef> = tchecker {
    let! variables = tchecker {
        match name with
        | { Alias = Some alias } ->
            let! source = getAliasedSource alias
            yield source |> getVariableDeclarations |> List.map (fun v -> { VarRef.Variable = v; Source = source })
        | { Alias = None } ->
            let! sources = getImplicitlyReferencedSources
            let! source = getFromContext (fun c -> c.CurrentSource)
            let sources = source :: (List.rev sources)
            yield sources |> List.collect (fun source -> source |> getVariableDeclarations |> List.map (fun v -> { VarRef.Variable = v; Source = source }))
    }

    match variables |> List.filter (fun vref -> vref.Variable.Name = name.Name) with
    | [] -> yield! fatalDiag $"Can't find variables with name \"%O{name}\""
    | [ variable ] -> yield variable
    | variables -> yield! fatalDiag $"Found more than 1 (%d{List.length variables}) variables with name \"%O{name}\""
}

let unwrapList (list: M<'a, 'b> list) : M<'a,  'b list> = (fun context ->
    let folder results m =
        match results with
        | Ok results ->
            match m context with
            | Ok (value, []) -> Ok (results @ [ value ])
            | Ok (_, diags) -> Error diags
            | Error diags -> Error diags
        | Error diags -> Error diags

    match list |> List.fold folder (Ok []) with
    | Ok results -> Ok (results, [])
    | Error diags -> Error diags
    )

let diags2Str (diags: Diagnostics) =
    let diags = diags |> List.map (fun d -> d.ToString ())
    String.concat "\n\t" diags
