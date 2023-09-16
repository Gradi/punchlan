module LibPunchLan.Parsing.LL1M

open LibPunchLan
open LibPunchLan.Addons
open LibPunchLan.Lexing
open LibPunchLan.ResultM
open FSharp.Quotations
open FSharp.Quotations.Patterns
open FSharp.Quotations.DerivedPatterns
open FSharp.Reflection

type Reader = SeqReader<Result<LexemeContainer, string>>

type M<'a> = Reader -> Result<'a, string>

type ParserM () =

    member _.Bind(m: M<'a>, f: 'a -> M<'b>) = (fun reader ->
        match m reader with
        | Ok value -> f value reader
        | Error error -> Error error )

    member _.Return value = (fun _ -> Ok value)

    member _.ReturnFrom value = value

    member _.Zero () = (fun _ -> Ok ())

    member _.Delay f = f

    member _.Run f = f ()

    member _.Combine (left: M<unit>, right: unit -> M<'a>) : M<'a> = (fun reader ->
        match left reader with
        | Ok () -> right () reader
        | Error error -> Error error)

let parser = ParserM ()

let parseError (input: LexemeContainer) msg : M<'a> = (fun (_: Reader) ->
    let message = sprintf msg
    let message = sprintf $"(row: %d{input.Row}, col: %d{input.Col}, lexeme: \"%A{input.Lexeme}\"): %s{message}"

    Error message)

let tryConsume (expected: Lexeme) (reader: Reader) = result {
    let! input = reader.Next ()

    match input.Lexeme with
    | actual when actual = expected -> return true
    | _ ->
        reader.Return (Ok input)
        return false
}

let consume (expected: Lexeme) (reader: Reader) = result {
    let! input = reader.Next ()

    match input.Lexeme with
    | actual when actual = expected -> return ()
    | _ ->
        reader.Return (Ok input)
        return! parseError input $"Expected \"%A{expected}\"." reader
}

let rec skipNewlines () = (fun (reader: Reader) -> result {
    match! reader.Next () with
    | { Lexeme = Lexeme.Newline } -> return! skipNewlines () reader
    | input ->
        reader.Return (Ok input)
        return ()
})

let next (reader: Reader) = reader.Next ()

let peek (reader: Reader) = reader.Peek ()

let returnLex (lex: LexemeContainer) (reader: Reader) =
    reader.Return (Ok lex)
    Ok ()

let readIdentifier (reader: Reader) = result {
    match! reader.Next () with
    | { Lexeme = Lexeme.Identifier id } -> return id
    | input ->
        reader.Return (Ok input)
        return! parseError input "Expected identifier" reader
}
