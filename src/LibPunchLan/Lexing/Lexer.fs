module rec LibPunchLan.Lexing.Lexer

open System.Text
open LibPunchLan
open LibPunchLan.Lexing

type Char =
    { Char: char
      Row: int
      Col: int }

type private LexerState =
    { CharBuf: Char list
      ReturnedChars: Char list
      State: Char option -> LexerState -> LexerResult }

type private LexerResult =  LexerState * Result<LexemeContainer, string> option


let private stateNone (state: LexerState) = (state, None)

let private stateOk (state: LexerState) (lex: Lexeme) (char: Char) =
    (state, Some <| Ok { LexemeContainer.Lexeme= lex; Row = char.Row; Col = char.Col })

let private stateError (state: LexerState) (char: Char) msg =
    let message = sprintf msg
    let message = sprintf $"(row: %d{char.Row}, col: %d{char.Col}): %s{message}"

    (state, Some <| Error message)

let private stateError' (state: LexerState) msg = stateError state { Char.Char = ' ' ; Row = -1; Col = -1} msg

let private stateAddChar (char: Char) (state: LexerState) =
    { state with CharBuf = state.CharBuf @ [ char ] }

let private stateReturnChar (char: Char) (state: LexerState) =
    { state with ReturnedChars = char :: state.ReturnedChars }

let private charsToStr (chars: Char list) =
    let builder = StringBuilder ()
    chars
    |> List.iter (fun c -> builder.Append c.Char |> ignore)

    builder.ToString ()



let private initialState (char: Char option) (state: LexerState) =
    match char with
    | None -> stateNone state

    | Some ({ Char = '(' } as char) -> stateOk state Lexeme.LParen char
    | Some ({ Char = ')' } as char) -> stateOk state Lexeme.RParen char
    | Some ({ Char = '[' } as char) -> stateOk state Lexeme.LSBracket char
    | Some ({ Char = ']' } as char) -> stateOk state Lexeme.RSBracket char
    | Some ({ Char = '{' } as char) -> stateOk state Lexeme.LCBracket char
    | Some ({ Char = '}' } as char) -> stateOk state Lexeme.RCBracket char
    | Some ({ Char = ':' } as char) -> stateOk state Lexeme.Colon char
    | Some ({ Char = ',' } as char) -> stateOk state Lexeme.Comma char
    | Some ({ Char = '\n' } as char) -> stateOk state Lexeme.Newline char

    | Some ({ Char = '+' } as char) -> stateOk state (Lexeme.Operator "+") char
    | Some ({ Char = '-' } as char) -> stateOk state (Lexeme.Operator "-") char
    | Some ({ Char = '*' } as char) -> stateOk state (Lexeme.Operator "*") char
    | Some ({ Char = '/' } as char) -> stateOk state (Lexeme.Operator "/") char
    | Some ({ Char = '&' } as char) -> stateOk state (Lexeme.Operator "&") char

    | Some ({ Char = '\"' } as char) -> stateNone { (stateAddChar char state) with State = stringLiteralState }
    | Some ({ Char = '\'' } as char) -> stateNone { (stateAddChar char state) with State = charLiteralState }

    | Some ({ Char = ch } as char) when List.contains ch [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] ->
        stateNone { (stateAddChar char state) with State = numberLiteralState }

    | Some ({ Char = ch } as char) when System.Char.IsLetter ch || ch = '_' ->
        stateNone { (stateAddChar char state) with State = keywordOrIdentifierState }

    | Some ({ Char = '.' } as char) -> stateNone { (stateAddChar char state) with State = dotSymbolState }

    | Some ({ Char = '<' } as char)
    | Some ({ Char = '=' } as char)
    | Some ({ Char = '>' } as char)
    | Some ({ Char = '!' } as char) -> stateNone { (stateAddChar char state) with State = equalNotEqualState }

    | Some { Char = ' ' }
    | Some { Char = '\t' }
    | Some { Char = '\r' } -> stateNone state

    | Some ({ Char = ch } as char) -> stateError state char $"Unknown char \'%c{ch}\'"

let private stringLiteralState (char: Char option) (state: LexerState) =
    let emitString () =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: string literal state with empty char buffer."
        | quote :: rest ->
            let str = charsToStr rest
            stateOk { state with CharBuf = []; State = initialState } (Lexeme.String str) quote

    match char with
    | None -> emitString ()
    | Some { Char = '\\' } -> stateNone { state with State = escapeCharState state.State }
    | Some { Char = '\"' } -> emitString ()
    | Some ({ Char = _ } as char) -> stateNone (stateAddChar char state)

let private charLiteralState (char: Char option) (state: LexerState) =
    let emitChar state =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: char literal state with empty char buffer."
        | [ { Char = '\'' } as char ] -> stateError state char "Unfinished char literal"
        | [ { Char = '\'' } as char ; { Char = '\'' } ] -> stateError state char "Empty char literal"
        | [ { Char = '\'' } ; { Char = ch } as char ; { Char = '\'' } ] ->
            stateOk { state with CharBuf = []; State = initialState } (Lexeme.Char ch) char
        | xs ->
            let xsstr = charsToStr xs
            stateError state (List.head xs) $"Bad char literal, expected one char in single quotes, but found %s{xsstr}"

    match char with
    | None -> emitChar state
    | Some { Char = '\\' } -> stateNone { state with State = escapeCharState state.State }
    | Some ({ Char = '\'' } as char )-> emitChar (stateAddChar char state)
    | Some ({ Char = _ } as char) -> stateNone (stateAddChar char state)

let private numberLiteralState (char: Char option) (state: LexerState) =
    let isValidCharNumber (ch: char) =
        match ch with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
        | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
        | 'x' | 'X' | '.' -> true
        | _ -> false

    let emitNumber (state: LexerState) =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: number literal state with emtpy buffer"
        | xs ->
            let str = charsToStr xs
            match NumberMod.tryParseNumber str with
            | Some number ->
                stateOk { state with CharBuf = []; State = initialState } (Lexeme.Number number) (List.head xs)
            | None -> stateError state (List.head xs) $"Can't parse value \"%s{str}\" into number."

    match char with
    | None -> emitNumber state
    | Some ({ Char = ch } as char) when isValidCharNumber ch ->
        stateNone (stateAddChar char state)
    | Some char -> emitNumber (stateReturnChar char state)

let private keywordOrIdentifierState (char: Char option) (state: LexerState) =
    let isValidChar ch = System.Char.IsLetter ch ||
                         System.Char.IsDigit ch ||
                         ch = '_'

    let emitBuffer (state: LexerState) =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: keyword or identifier state with empty char buffer"
        | xs ->
            let str = charsToStr xs
            let lexeme = stringToKeywordOrIdentifier str
            stateOk { state with CharBuf = []; State = initialState } lexeme (List.head xs)

    match char with
    | None -> emitBuffer state
    | Some ({ Char = ch } as char) when isValidChar ch -> stateNone (stateAddChar char state)
    | Some char -> emitBuffer (stateReturnChar char state)

let private dotSymbolState (char: Char option) (state: LexerState) =
    let emitDot (state: LexerState) =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: dot symbol state with empty char buffer"
        | xs ->
            match charsToStr xs with
            | "." -> stateOk { state with CharBuf = []; State = initialState } Lexeme.Dot (List.head xs)
            | ".." -> stateOk { state with CharBuf = []; State = initialState } Lexeme.DoubleDot (List.head xs)
            | xs -> stateError state (List.head state.CharBuf) $"Only \'.\' or \'..\' dots allowed, but not this: %s{xs}"

    match char with
    | None -> emitDot state
    | Some ({ Char = '.' } as char) -> stateNone (stateAddChar char state)
    | Some char -> emitDot (stateReturnChar char state)

let private equalNotEqualState (char: Char option) (state: LexerState) =
    let emit (state: LexerState) =
        match state.CharBuf with
        | [] -> failwith "Unexpected state: equal not equal state with empty char buffer."
        | xs ->
            let str = charsToStr xs
            let lexeme =
                match str with
                | "<" -> Some Lexeme.LABracket
                | "<<" -> Some <| Lexeme.Operator "<<"
                | "<=" -> Some Lexeme.LessThanOrEqual
                | ">" -> Some Lexeme.RABracket
                | ">>" -> Some <| Lexeme.Operator ">>"
                | ">=" -> Some Lexeme.GreaterThanOrEqual
                | "=" -> Some Lexeme.Equal
                | "==" -> Some Lexeme.DEqual
                | "!=" -> Some Lexeme.NotEqual
                | _ -> None

            match lexeme with
            | Some lexeme -> stateOk { state with CharBuf = []; State = initialState } lexeme (List.head xs)
            | None -> stateError state (List.head xs) $"Bad operator string: %s{str}."

    match char with
    | None -> emit state
    | Some ({ Char = '<' } as char)
    | Some ({ Char = '=' } as char)
    | Some ({ Char = '>' } as char)
    | Some ({ Char = '!' } as char) -> stateNone (stateAddChar char state)
    | Some char -> emit (stateReturnChar char state)

let private escapeCharState prevState (char: Char option) (state: LexerState) =
    match char with
    | None -> stateError' state "Unexpected end of file"
    | Some char ->
        let finalChar =
            match char.Char with
            | 'a' -> '\a'
            | 'b' -> '\b'
            | 'f' -> '\f'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | 'v' -> '\v'
            | c -> c
        stateNone { (stateAddChar { char with Char = finalChar } state) with State = prevState }

let private stringToKeywordOrIdentifier str =
    match str with
    | "open" -> Lexeme.Keyword Keyword.Open
    | "as" -> Lexeme.Keyword Keyword.As
    | "func" -> Lexeme.Keyword Keyword.Func
    | "endfunc" -> Lexeme.Keyword Keyword.EndFunc
    | "struct" -> Lexeme.Keyword Keyword.Struct
    | "endstruct" -> Lexeme.Keyword Keyword.EndStruct
    | "union" -> Lexeme.Keyword Keyword.Union
    | "endunion" -> Lexeme.Keyword Keyword.EndUnion
    | "if" -> Lexeme.Keyword Keyword.If
    | "then" -> Lexeme.Keyword Keyword.Then
    | "elseif" -> Lexeme.Keyword Keyword.ElseIf
    | "else" -> Lexeme.Keyword Keyword.Else
    | "fi" -> Lexeme.Keyword Keyword.Fi
    | "while" -> Lexeme.Keyword Keyword.While
    | "do" -> Lexeme.Keyword Keyword.Do
    | "endwhile" -> Lexeme.Keyword Keyword.EndWhile
    | "for" -> Lexeme.Keyword Keyword.For
    | "in" -> Lexeme.Keyword Keyword.In
    | "endfor" -> Lexeme.Keyword Keyword.EndFor
    | "return" -> Lexeme.Keyword Keyword.Return
    | "var" -> Lexeme.Keyword Keyword.Var
    | "extern" -> Lexeme.Keyword Keyword.Extern
    | "export" -> Lexeme.Keyword Keyword.Export
    | "defer" -> Lexeme.Keyword Keyword.Defer
    | "enddefer" -> Lexeme.Keyword Keyword.EndDefer
    | "int8" -> Lexeme.Keyword Keyword.Int8
    | "uint8" -> Lexeme.Keyword Keyword.Uint8
    | "int16" -> Lexeme.Keyword Keyword.Int16
    | "uint16" -> Lexeme.Keyword Keyword.Uint16
    | "int32" -> Lexeme.Keyword Keyword.Int32
    | "uint32" -> Lexeme.Keyword Keyword.Uint32
    | "int64" -> Lexeme.Keyword Keyword.Int64
    | "uint64" -> Lexeme.Keyword Keyword.Uint64
    | "double" -> Lexeme.Keyword Keyword.Double
    | "float" -> Lexeme.Keyword Keyword.Float
    | "char" -> Lexeme.Keyword Keyword.Char
    | "bool" -> Lexeme.Keyword Keyword.Bool
    | "void" -> Lexeme.Keyword Keyword.Void
    | "pointer" -> Lexeme.Keyword Keyword.Pointer
    | "const" -> Lexeme.Keyword Keyword.Const
    | "true" -> Lexeme.Keyword Keyword.True
    | "false" -> Lexeme.Keyword Keyword.False
    | "and" -> Lexeme.Keyword Keyword.And
    | "or" -> Lexeme.Keyword Keyword.Or
    | "not" -> Lexeme.Keyword Keyword.Not
    | "xor" -> Lexeme.Keyword Keyword.Xor
    | "sizeof" -> Lexeme.Keyword Keyword.Sizeof
    | "addrof" -> Lexeme.Keyword Keyword.Addrof
    | "deref" -> Lexeme.Keyword Keyword.Deref
    | "cast" -> Lexeme.Keyword Keyword.Cast
    | str -> Lexeme.Identifier str



let textReaderToCharSeq (reader: System.IO.TextReader) = seq {
    let mutable input = reader.Read ()

    while input <> -1 do
        yield (char input)
        input <- reader.Read ()
}

let indexedChars (chars: char seq) = seq {
    let mutable row = 1
    let mutable col = 1

    for char in chars do
        yield { Char.Char = char; Row = row; Col = col }

        col <- col + 1
        if char = '\n' then
            row <- row + 1
            col <- 1
}

let tokenize (chars: Char seq) : Result<LexemeContainer, string> seq =
    seq {
        let mutable state =
            { CharBuf = []
              ReturnedChars = []
              State = initialState }

        let rec returnChars () = seq {
            match state.ReturnedChars with
            | [] -> ()
            | head :: rest ->
                state <- { state with ReturnedChars = rest }

                let newState, resultOption = state.State (Some head) state
                state <- newState

                match resultOption with
                | Some result -> yield result
                | None -> ()

                yield! returnChars ()
        }

        for char in chars do
            yield! returnChars ()
            assert (List.isEmpty state.ReturnedChars)

            let newState, resultOption = state.State (Some char) state
            state <- newState

            match resultOption with
            | Some result -> yield result
            | None -> ()

        yield! returnChars ()
        assert (List.isEmpty state.ReturnedChars)

        let _, resultOption = state.State None state
        match resultOption with
        | Some result -> yield result
        | None -> ()

        yield Ok { LexemeContainer.Lexeme = Lexeme.EndOfFile; Row = -1; Col = -1 }
    }
    |> MSeq.takeUntilFirstError

let tokenizeFromString (str: string) =
    let chars = textReaderToCharSeq (new System.IO.StringReader(str))
    let indexesChars = indexedChars chars

    tokenize indexesChars
