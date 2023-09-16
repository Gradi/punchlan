module LibPunchLan.MSeq

let takeUntilFirstError (collection: Result<'a, 'b> seq) = seq {
    use enumerator = collection.GetEnumerator ()
    let mutable iterate = true

    while iterate && enumerator.MoveNext () do
        let current = enumerator.Current
        yield current
        iterate <- Result.isOk current
}
