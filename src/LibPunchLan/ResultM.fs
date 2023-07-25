module LibPunchLan.ResultM

let resultf str = Result.Error (sprintf str)

type ResultM () =

    member _.Bind (m, f) = Result.bind f m

    member _.Return item = Ok item

    member _.ReturnFrom item = item

    member _.Using (m, f) = f m

let result = ResultM ()
