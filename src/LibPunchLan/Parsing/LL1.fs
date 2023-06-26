module LibPunchLan.Parsing.LL1

open LibPunchLan.Addons
open LibPunchLan.ResultM
open LibPunchLan.Lexing

type private Reader = Result<LexemeContainer, string> SeqReader

let private parseError state  (lexeme: LexemeContainer option) msg =
    match lexeme with
    | Some lexeme ->
        Error (sprintf $"(%d{lexeme.RowStart}, %d{lexeme.ColStart}): Parse error in \'%s{state}\' state, lexeme: %A{lexeme}, msg: %s{msg}.")
    | None -> Error (sprintf $"(): Parse error in \'%s{state}\', lexeme \'EOF\', msg: %s{msg}")

let private consumeLexeme (lexeme: Lexeme) state (reader: Reader) = result {
    let! input = reader.TryNext ()
    match input with
    | Some { Lexeme = curLexeme } when curLexeme = lexeme -> return ()
    | container -> return! parseError state container (sprintf $"Expected a lexeme \'%A{lexeme}\' to consume.")
}


let rec parseOpenDirective (reader: Reader): Result<OpenDirective list, string> =

    let rec parsePath () : Result<string, string> = result {

        let parsePath' () : Result<string option, string> = result {
            let! input = reader.TryNext ()
            match input with
            | Some { Lexeme = Operator "/" } ->
                let! path = parsePath ()
                return Some path
            | Some item ->
                reader.Return (Ok item)
                return None
            | None ->
                return None
        }

        let! input = reader.TryNext ()
        match input with
        | Some { Lexeme = Identifier id } ->
            match! parsePath' () with
            | Some path -> return (sprintf $"%s{id}/%s{path}")
            | None -> return id

        | _ -> return! (parseError "PATH" input "Expected Identifier")
    }

    let parseAsId () = result {
        let! input = reader.TryNext ()
        match input with
        | Some { Lexeme = Keyword As } ->
            let! input = reader.TryNext ()
            match input with
            | Some { Lexeme = Identifier id }  -> return Some id
            | _ -> return! (parseError "AS_ID" input "Expected Identifier")
        | Some input ->
            reader.Return (Ok input)
            return None
        | None ->
            return None
    }

    result {
        let! input = reader.TryNext ()
        match input with
        | Some { Lexeme = Keyword Open } ->
            let! path = parsePath ()
            let! alias = parseAsId ()
            let result = { Path = path; Alias = alias }
            do! consumeLexeme Lexeme.Newline "OPEN_DIRECTIVE" reader

            let! nextOpenDir = parseOpenDirective reader
            return result :: nextOpenDir
        | Some input ->
            reader.Return (Ok input)
            return []
        | None ->
            return []
    }
