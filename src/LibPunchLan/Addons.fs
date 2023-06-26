module LibPunchLan.Addons

open System

type SeqReader<'a> (seq: 'a seq) as this =

    let mutable isDisposed = false
    let enumerator = lazy(seq.GetEnumerator ())
    let stack = System.Collections.Generic.Stack<'a> ()

    member _.Return item =
        this.CheckDisposed ()
        stack.Push item

    member _.TryReturn item =
        this.CheckDisposed ()
        match item with
        | Some item -> this.Return item
        | None -> ()

    member _.TryNext () =
        this.CheckDisposed ()

        match stack.TryPop () with
        | true, item -> Some item
        | false, _ ->
            if enumerator.Value.MoveNext () then
                Some enumerator.Value.Current
            else
                None

    member _.TryPeek () =
        this.CheckDisposed ()

        match this.TryNext () with
        | Some item ->
            this.Return item
            Some item
        | None -> None

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
