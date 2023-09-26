module LibPunchLan.ResultM

let resultf str = Result.Error (sprintf str)

type ResultM () =

    member _.Bind (m, f) = Result.bind f m

    member _.Return item = Ok item

    member _.ReturnFrom item = item

    member _.Using (m, f) = f m

    member _.Zero () = Ok ()

    member _.Delay f = f

    member _.Run f = f ()

    member _.For(inputs: 'a seq, f: 'a -> Result<unit, 'c>) =
        use enumerator = inputs.GetEnumerator ()
        let mutable result = Ok ()

        while Result.isOk result && enumerator.MoveNext () do
            let current = enumerator.Current
            result <- f current

        result


let result = ResultM ()
