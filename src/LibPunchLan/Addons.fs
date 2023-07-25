module LibPunchLan.Addons

open System

type SeqReader<'a> (seq: 'a seq) as this =

    let mutable isDisposed = false
    let enumerator = lazy(seq.GetEnumerator ())
    let stack = System.Collections.Generic.Stack<'a> ()

    member _.Return item =
        this.CheckDisposed ()
        stack.Push item

    member _.TryNext () =
        this.CheckDisposed ()

        match stack.TryPop () with
        | true, item -> Some item
        | false, _ ->
            if enumerator.Value.MoveNext () then
                Some enumerator.Value.Current
            else
                None

    member _.Next () =
        this.CheckDisposed ()
        match this.TryNext () with
        | Some item -> item
        | None -> failwith "End of sequence or empty sequence."

    member _.Peek () =
        let item = this.Next()
        this.Return item
        item

    member private _.CheckDisposed () =
        if isDisposed then raise (ObjectDisposedException (nameof SeqReader))

    interface IDisposable with
        member _.Dispose () =
            if isDisposed then ()
            else
                try
                    if enumerator.IsValueCreated then enumerator.Value.Dispose ()
                finally
                    isDisposed <- true
