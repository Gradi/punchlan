module LibPunchLan.Lexing.Lexer

open System.Text
open LibPunchLan.Lexing
open System.IO
open System.Collections.Generic
open LibPunchLan.Addons

type Reader(textReader: TextReader) =
    let returnedSymbols = Stack<Char> ()
    let mutable rowCount: int = 0
    let mutable colCount: int = 0

    member _.Return char = returnedSymbols.Push char

    member _.Next () =
        match returnedSymbols.TryPop () with
        | true, char -> Some char
        | false, _ ->
            match textReader.Read () with
            | -1 -> None
            | value ->
                let char = char value
                let result = { Char.Char = char; Row = rowCount; Col = colCount; }
                colCount <- colCount + 1

                if char = '\n' then
                    rowCount <- rowCount + 1
                    colCount <- 0

                Some result

    static member FromString str =
        let strReader = new StringReader(str)
        Reader(strReader)


let stringToKeywordOrIdentifier str =
    match str with
    | "open" -> Lexeme.Keyword <| Keyword.Open
    | "as" -> Lexeme.Keyword <| Keyword.As
    | "func" -> Lexeme.Keyword <| Keyword.Func
    | "endfunc" -> Lexeme.Keyword <| Keyword.EndFunc
    | "struct" -> Lexeme.Keyword <| Keyword.Struct
    | "endstruct" -> Lexeme.Keyword <| Keyword.EndStruct
    | "union" -> Lexeme.Keyword <| Keyword.Union
    | "endunion" -> Lexeme.Keyword <| Keyword.EndUnion
    | "enum"  -> Lexeme.Keyword <| Keyword.Enum
    | "endenum" -> Lexeme.Keyword <| Keyword.EndEnum
    | "if" -> Lexeme.Keyword <| Keyword.If
    | "else" -> Lexeme.Keyword <| Keyword.Else
    | "fi" -> Lexeme.Keyword <| Keyword.Fi
    | "switch" -> Lexeme.Keyword <| Keyword.Switch
    | "case" -> Lexeme.Keyword <| Keyword.Case
    | "break" -> Lexeme.Keyword <| Keyword.Break
    | "default" -> Lexeme.Keyword <| Keyword.Default
    | "endswitch" -> Lexeme.Keyword <| Keyword.EndSwitch
    | "while" -> Lexeme.Keyword <| Keyword.While
    | "endwhile" -> Lexeme.Keyword <| Keyword.EndWhile
    | "for" -> Lexeme.Keyword <| Keyword.For
    | "In" -> Lexeme.Keyword <| Keyword.In
    | "endfor" -> Lexeme.Keyword <| Keyword.EndFor
    | "private" -> Lexeme.Keyword <| Keyword.Private
    | "return" -> Lexeme.Keyword <| Keyword.Return
    | "var" -> Lexeme.Keyword <| Keyword.Var
    | "extern" -> Lexeme.Keyword <| Keyword.Extern
    | "int8" -> Lexeme.Keyword <| Keyword.Int8
    | "uint8" -> Lexeme.Keyword <| Keyword.Uint8
    | "int16" -> Lexeme.Keyword <| Keyword.Int16
    | "uint16" -> Lexeme.Keyword <| Keyword.Uint16
    | "int32" -> Lexeme.Keyword <| Keyword.Int32
    | "uint32" -> Lexeme.Keyword <| Keyword.Uint32
    | "double" -> Lexeme.Keyword <| Keyword.Double
    | "float" -> Lexeme.Keyword <| Keyword.Float
    | "char" -> Lexeme.Keyword <| Keyword.Char
    | "bool" -> Lexeme.Keyword <| Keyword.Bool
    | "sizet" -> Lexeme.Keyword <| Keyword.Sizet
    | "void" -> Lexeme.Keyword <| Keyword.Void
    | "pointer" -> Lexeme.Keyword <| Keyword.Pointer
    | "const" -> Lexeme.Keyword <| Keyword.Const
    | "and" -> Lexeme.Keyword <| Keyword.And
    | "or" -> Lexeme.Keyword <| Keyword.Or
    | "not" -> Lexeme.Keyword <| Keyword.Not
    | "xor" -> Lexeme.Keyword <| Keyword.Xor
    | value -> Lexeme.Identifier value

type private LexerState = Char option -> Result<LexemeContainer option, string>

type private Tokenizer(reader: Reader) as this =

    let mutable state: LexerState = this.InitialState
    let mutable charBuffer: Char list = []

    let ok charStart charEnd lexeme = Ok <| Some
                                                { LexemeContainer.Lexeme = lexeme
                                                  RowStart = charStart.Row
                                                  RowEnd = charEnd.Row
                                                  ColStart = charStart.Col
                                                  ColEnd = charEnd.Col }
    let none = Ok None

    let failUnknownChar char =
        resultf $"Unknown character '%c{char.Char}' at (%d{char.Row}, %d{char.Col})."

    let failUnexpectedEOF = resultf "Unexpected EOF."

    let clearCharBuffer () = charBuffer <- []

    let charBufferToStr () =
        let sb = StringBuilder (List.length charBuffer)
        charBuffer |> List.iter (fun ch -> sb.Append ch.Char |> ignore)
        sb.ToString ()

    member private _.InitialState char =
        match char with
        | None -> failUnexpectedEOF
        | Some char ->
            match char.Char with
            | '(' -> ok char char Lexeme.LParen
            | ')' -> ok char char Lexeme.RParen
            | '[' -> ok char char Lexeme.LSBracket
            | ']' -> ok char char Lexeme.RSBracket
            | '<' -> ok char char Lexeme.LABracket
            | '>' -> ok char char Lexeme.RABracket
            | ':' -> ok char char Lexeme.Colon
            | ',' -> ok char char Lexeme.Comma
            | '-' -> ok char char (Lexeme.Operator "-")
            | '+' -> ok char char (Lexeme.Operator "+")
            | '*' -> ok char char (Lexeme.Operator "*")
            | '/' -> ok char char (Lexeme.Operator "/")
            | '|' -> ok char char (Lexeme.Operator "|")
            | '&' -> ok char char (Lexeme.Operator "&")

            | '='
            | '!' ->
                charBuffer <- [ char ]
                state <- this.OperatorState
                none

            | '\n' -> ok char char Lexeme.Newline

            | value when System.Char.IsLetter value || value = '_' ->
                charBuffer <- [ char ]
                state <-  this.KeywordOrIdentifierState
                none

            | value when System.Char.IsNumber value ->
                charBuffer <- [ char ]
                state <- this.NumberState
                none

            | ' '
            | '\t'
            | '\r' -> none

            | _ -> failUnknownChar char

    member _.KeywordOrIdentifierState inputChar =
        let emitCurrentKeyOrId () =
            let startChar = List.head charBuffer
            let endChar = List.last charBuffer
            let string = charBufferToStr ()

            clearCharBuffer ()
            ok startChar endChar (stringToKeywordOrIdentifier string)

        let isPermittedChar char =
            System.Char.IsLetter char ||
            System.Char.IsNumber char ||
            char = '_'

        match inputChar, charBuffer with
        | None, [] -> none
        | None, _ -> emitCurrentKeyOrId ()
        | Some char, _ when isPermittedChar char.Char ->
            charBuffer <- charBuffer @ [ char ]
            none
        | Some _, [] -> resultf "Invalid state: KeywordOrIdentifierState with empty buffer."
        | Some char, _ ->
            let result = emitCurrentKeyOrId ()
            state <- this.InitialState
            reader.Return char
            result

    member _.NumberState inputChar =
        let emitCurrentNumber () =
            let numberStr = charBufferToStr ()
            let startChar = List.head charBuffer
            let endChar = List.last charBuffer

            clearCharBuffer ()

            match NumberMod.tryParseNumber numberStr with
            | None -> resultf $"Can't parse number \'%s{numberStr}\'."
            | Some number -> ok startChar endChar (Lexeme.Number number)

        match (inputChar, charBuffer) with
        | None, [] -> none
        | None, _ -> emitCurrentNumber ()

        | Some char, _ ->
            match char.Char with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
            | 'x'
            | '-' | '.' ->
                charBuffer <- charBuffer @ [ char ]
                none
            | _ ->
                match charBuffer with
                | [] -> resultf "Invalid state: NumberState with empty buffer."
                | _ ->
                    let result = emitCurrentNumber ()
                    reader.Return char
                    result

    member _.OperatorState inputChar =
        let emitCurrentOperator () =
            let operatorStr = charBufferToStr ()
            let start = List.head charBuffer
            let endChar = List.last charBuffer

            clearCharBuffer ()

            match operatorStr with
            | "=" -> ok start endChar Lexeme.Equal
            | "==" -> ok start endChar Lexeme.DEqual
            | "!=" -> ok start endChar Lexeme.NotEqual
            | "<=" -> ok start endChar Lexeme.LessThanOrEqual
            | ">=" -> ok start endChar Lexeme.GreaterThanorEqual
            | value -> resultf $"Unknown operator \'%s{value}\'."

        match inputChar, charBuffer with
        | None, [] -> none
        | None, _ -> emitCurrentOperator ()

        | Some ({ Char = '=' } as char), _ ->
            charBuffer <- charBuffer @ [ char ]
            emitCurrentOperator ()
        | Some _, [] -> resultf "Invalid state: OperatorState with empty buffer."
        | Some char, _ ->
            let result = emitCurrentOperator ()
            reader.Return char
            state <- this.InitialState
            result

    member _. GetLexemes () : Result<LexemeContainer, string> seq =
        seq {
            let mutable inputChar = reader.Next ()

            while Option.isSome inputChar do
                yield state inputChar
                inputChar <- reader.Next ()

            yield state None
        } |> Seq.choose (fun result ->
            match result with
            | Error str -> Some (Error str)
            | Ok None -> None
            | Ok (Some lexeme) -> Some (Ok lexeme))


let tokenize reader =
    let tokenizer = Tokenizer reader
    tokenizer.GetLexemes ()
