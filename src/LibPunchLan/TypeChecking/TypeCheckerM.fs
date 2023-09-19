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
                Function = context.CurrentFunction
                Message = msg }
    Ok ((),  [ dia ])

let fatalDiag msg (context: SourceContext) =
    match diag msg context with
    | Ok ((), diags) -> Error diags
    | Error diags -> Error diags

let context ctx = Ok (ctx, [])

let getFromContext f ctx = Ok (f ctx, [])

let checkWithContext context m = (fun _ -> m context)

let getFuncFromContext (context: SourceContext) =
    match context.CurrentFunction with
    | Some func -> Ok (func, [])
    | None -> failwith "Context's current function is not set. Should not happen."

let getAliasedSource (alias: string) = tchecker {
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

let getImplicitlyReferencedSources = tchecker {
    let! context = context

    let sources =
        context.CurrentSource.OpenDirectives
        |> List.filter (fun od -> Option.isNone od.Alias)
        |> List.map (fun od -> od.Path)
        |> List.collect (fun path -> context.Program.Sources |> List.filter (fun source -> source.Filename = path))

    yield sources
}

let locateTypeDecl (name: DotString) = tchecker {
    let! types = tchecker {
        match name with
        | { Alias = Some alias } ->
            let! source = getAliasedSource alias

            source |> getTypeDeclarations

        | { Alias = None } ->
            let! sources = getImplicitlyReferencedSources
            let! source = getFromContext (fun c -> c.CurrentSource)
            let sources = source :: (List.rev sources)

            sources |> List.collect getTypeDeclarations
    }

    let types =
        types
        |> List.filter (fun t -> t.Name = name.Name)

    match types with
    | [] -> yield! fatalDiag $"Can't find type named \"%O{name}\""
    | [ typ ] -> yield typ
    | types -> yield! fatalDiag $"Found more than 1 (%d{List.length types}) types named \"%O{name}\""
}

let locateFunctionDecl (name: DotString) = tchecker {
    let! funcs = tchecker {
        match name with
        | { Alias = Some alias } ->
            let! source = getAliasedSource alias

            yield source |> getFunctionDeclarations
        | { Alias = None } ->
            let! source = getFromContext (fun c -> c.CurrentSource)
            let! sources = getImplicitlyReferencedSources
            let sources = source :: (List.rev sources)

            yield
                sources
                |> List.collect (fun s -> s |> getFunctionDeclarations)
    }

    match funcs |> List.filter (fun f -> f.Name = name.Name) with
    | [] -> yield! fatalDiag $"Can't find function named \"%O{name}"
    | [ f ] -> yield f
    | fs -> yield! fatalDiag $"Found more than 1 (%d{List.length fs}) functions named \"%O{name}\""
}
