module LibPunchLan.ResultM

let resultf str = Result.Error (sprintf str)

type ResultM () =

    member _.Bind (m, f) = Result.bind f m

    member _.Bind(m: Result<'a, string> option, f: 'a option -> Result<'b, string>) =
        match m with
        | Some (Ok result) -> f (Some result)
        | Some (Error msg) -> Error msg
        | None -> f None

    member _.Return item = Ok item

    member _.ReturnFrom item = item

let result = ResultM ()
