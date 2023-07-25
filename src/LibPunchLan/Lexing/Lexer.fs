module LibPunchLan.Lexing.Lexer

open System.Text
open LibPunchLan.Lexing
open LibPunchLan.ResultM
open LibPunchLan.Addons

let stringToKeywordOrIdentifier str =
    match str with
    | "and" -> Lexeme.Keyword          Keyword.And
    | "as" -> Lexeme.Keyword           Keyword.As
    | "bool" -> Lexeme.Keyword         Keyword.Bool
    | "char" -> Lexeme.Keyword         Keyword.Char
    | "const" -> Lexeme.Keyword        Keyword.Const
    | "defer" -> Lexeme.Keyword        Keyword.Defer
    | "double" -> Lexeme.Keyword       Keyword.Double
    | "else" -> Lexeme.Keyword         Keyword.Else
    | "elseif" -> Lexeme.Keyword       Keyword.ElseIf
    | "enddefer" -> Lexeme.Keyword     Keyword.EndDefer
    | "endenum" -> Lexeme.Keyword      Keyword.EndEnum
    | "endfor" -> Lexeme.Keyword       Keyword.EndFor
    | "endfunc" -> Lexeme.Keyword      Keyword.EndFunc
    | "endstruct" -> Lexeme.Keyword    Keyword.EndStruct
    | "endunion" -> Lexeme.Keyword     Keyword.EndUnion
    | "endwhile" -> Lexeme.Keyword     Keyword.EndWhile
    | "enum"  -> Lexeme.Keyword        Keyword.Enum
    | "extern" -> Lexeme.Keyword       Keyword.Extern
    | "false" -> Lexeme.Keyword        Keyword.False
    | "fi" -> Lexeme.Keyword           Keyword.Fi
    | "float" -> Lexeme.Keyword        Keyword.Float
    | "for" -> Lexeme.Keyword          Keyword.For
    | "func" -> Lexeme.Keyword         Keyword.Func
    | "if" -> Lexeme.Keyword           Keyword.If
    | "in" -> Lexeme.Keyword           Keyword.In
    | "int16" -> Lexeme.Keyword        Keyword.Int16
    | "int32" -> Lexeme.Keyword        Keyword.Int32
    | "int8" -> Lexeme.Keyword         Keyword.Int8
    | "not" -> Lexeme.Keyword          Keyword.Not
    | "open" -> Lexeme.Keyword         Keyword.Open
    | "or" -> Lexeme.Keyword           Keyword.Or
    | "pointer" -> Lexeme.Keyword      Keyword.Pointer
    | "private" -> Lexeme.Keyword      Keyword.Private
    | "return" -> Lexeme.Keyword       Keyword.Return
    | "sizet" -> Lexeme.Keyword        Keyword.Sizet
    | "struct" -> Lexeme.Keyword       Keyword.Struct
    | "true" -> Lexeme.Keyword         Keyword.True
    | "uint16" -> Lexeme.Keyword       Keyword.Uint16
    | "uint32" -> Lexeme.Keyword       Keyword.Uint32
    | "uint8" -> Lexeme.Keyword        Keyword.Uint8
    | "union" -> Lexeme.Keyword        Keyword.Union
    | "var" -> Lexeme.Keyword          Keyword.Var
    | "void" -> Lexeme.Keyword         Keyword.Void
    | "while" -> Lexeme.Keyword        Keyword.While
    | "xor" -> Lexeme.Keyword          Keyword.Xor
    | "then" -> Lexeme.Keyword         Keyword.Then
    | "do" -> Lexeme.Keyword           Keyword.Do
    | value -> Lexeme.Identifier value

type private LexerState = char option -> unit

type private Tokenizer(reader: char SeqReader) as this =

    let mutable state: LexerState = this.InitialState
    let mutable charBuffer: char list = []
    let mutable nextLexeme: Result<Lexeme, string> option = None

    let ok lexeme =
        match nextLexeme with
        | Some _ -> failwith "Lexer: Current lexeme is not None."
        | None ->
            nextLexeme <- Some <| Ok lexeme

    let failUnknownChar char =
        nextLexeme <- Some <| resultf $"Lexer: Unknown character '%c{char}'."

    let fail (str: Printf.StringFormat<string>) =
        nextLexeme <- Some <| resultf $"Lexer: %s{sprintf str}"

    let clearCharBuffer () = charBuffer <- []

    let charBufferToStr () =
        let sb = StringBuilder (List.length charBuffer)
        charBuffer |> List.iter (fun ch -> sb.Append ch |> ignore)
        sb.ToString ()

    member private _.InitialState char =
        match char with
        | None -> ()
        | Some char ->
            match char with
            | '(' -> ok Lexeme.LParen
            | ')' -> ok Lexeme.RParen
            | '[' -> ok Lexeme.LSBracket
            | ']' -> ok Lexeme.RSBracket
            | '{' -> ok Lexeme.LCBracket
            | '}' -> ok Lexeme.RCBracket
            | '<' -> ok Lexeme.LABracket
            | '>' -> ok Lexeme.RABracket
            | ':' -> ok Lexeme.Colon
            | ',' -> ok Lexeme.Comma
            | '-' -> ok (Lexeme.Operator "-")
            | '+' -> ok (Lexeme.Operator "+")
            | '*' -> ok (Lexeme.Operator "*")
            | '/' -> ok (Lexeme.Operator "/")
            | '|' -> ok (Lexeme.Operator "|")
            | '&' -> ok (Lexeme.Operator "&")

            | '\"' ->
                charBuffer <- [ char ]
                state <- this.StringState

            | '\'' ->
                charBuffer <- [ char ]
                state <- this.CharState

            | '='
            | '!' ->
                charBuffer <- [ char ]
                state <- this.OperatorState

            | '.' ->
                charBuffer <- [ char ]
                state <- this.DotState

            | '\n' -> ok Lexeme.Newline

            | value when System.Char.IsLetter value || value = '_' ->
                charBuffer <- [ char ]
                state <-  this.KeywordOrIdentifierState

            | value when System.Char.IsNumber value ->
                charBuffer <- [ char ]
                state <- this.NumberState

            | ' '
            | '\t'
            | '\r' -> ()

            | _ -> failUnknownChar char

    member _.KeywordOrIdentifierState inputChar =
        let emitCurrentKeyOrId () =
            let string = charBufferToStr ()
            clearCharBuffer ()
            ok (stringToKeywordOrIdentifier string)

        let isPermittedChar char =
            System.Char.IsLetter char ||
            System.Char.IsNumber char ||
            char = '_'

        match inputChar, charBuffer with
        | None, [] -> ()
        | None, _ -> emitCurrentKeyOrId ()
        | Some char, _ when isPermittedChar char ->
            charBuffer <- charBuffer @ [ char ]
        | Some _, [] -> fail "Invalid state: KeywordOrIdentifierState with empty buffer."
        | Some char, _ ->
            let result = emitCurrentKeyOrId ()
            state <- this.InitialState
            reader.Return char
            result

    member _.NumberState inputChar =
        let emitCurrentNumber () =
            let numberStr = charBufferToStr ()
            clearCharBuffer ()

            match NumberMod.tryParseNumber numberStr with
            | None -> fail $"Can't parse number \'%s{numberStr}\'."
            | Some number -> ok (Lexeme.Number number)

        match (inputChar, charBuffer) with
        | None, [] -> ()
        | None, _ -> emitCurrentNumber ()

        | Some char, _ ->
            match char with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
            | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
            | 'x'
            | '-' | '.' ->
                charBuffer <- charBuffer @ [ char ]
            | _ ->
                match charBuffer with
                | [] -> fail "Invalid state: NumberState with empty buffer."
                | _ ->
                    let result = emitCurrentNumber ()
                    reader.Return char
                    state <- this.InitialState
                    result

    member _.OperatorState inputChar =
        let emitCurrentOperator () =
            let operatorStr = charBufferToStr ()
            clearCharBuffer ()

            match operatorStr with
            | "=" -> ok Lexeme.Equal
            | "==" -> ok Lexeme.DEqual
            | "!=" -> ok Lexeme.NotEqual
            | "<=" -> ok Lexeme.LessThanOrEqual
            | ">=" -> ok Lexeme.GreaterThanOrEqual
            | value -> fail $"Unknown operator \'%s{value}\'."

        match inputChar, charBuffer with
        | None, [] -> ()
        | None, _ -> emitCurrentOperator ()

        | Some ('=' as char), _ ->
            charBuffer <- charBuffer @ [ char ]
            emitCurrentOperator ()
        | Some _, [] -> fail "Invalid state: OperatorState with empty buffer."
        | Some char, _ ->
            let result = emitCurrentOperator ()
            reader.Return char
            state <- this.InitialState
            result

    member _.StringState inputChar =
        let emitCurrentStr () =
            charBuffer <- List.tail charBuffer // To remove first '"' char
            let str = charBufferToStr ()
            clearCharBuffer ()
            ok (Lexeme.String str)

        match (inputChar, charBuffer) with
        | None, []  -> ()
        | None, _ -> emitCurrentStr ()

        | Some '\\', _ ->
            state <- this.EscapeSeqState this.StringState

        | Some '\"', _ ->
            let result = emitCurrentStr ()
            state <- this.InitialState
            result

        | Some ch, _ ->
            charBuffer <- charBuffer @ [ ch ]

    member _.EscapeSeqState nextState inputChar =
        match inputChar with
        | Some 'n' ->
            charBuffer <-  charBuffer @ [ '\n' ]
            state <- nextState
        | Some 'r' ->
            charBuffer <- charBuffer @ [ '\r' ]
            state <- nextState
        | Some 't' ->
            charBuffer <- charBuffer @ [ '\t' ]
            state <- nextState
        | Some ch ->
            charBuffer <- charBuffer @ [ ch ]
            state <- nextState
        | None -> fail "Unexpected EOF."

    member _.CharState inputChar =
        let emitCurrentChar () =
            let str = charBufferToStr ()
            clearCharBuffer ()
            ok (Lexeme.Char str[0])

        match charBuffer, inputChar with
        | [], None -> ()
        | [ _ ], None -> emitCurrentChar ()

        | ['\'' ], Some '\\' ->
            charBuffer <- []
            state <- this.EscapeSeqState this.CharState

        | [ _ ], Some '\'' ->
            let result = emitCurrentChar ()
            state <- this.InitialState
            result
        | ['\'' ], Some ch ->
            charBuffer <- [ ch ]
        | xs, input -> fail $"Error on parsing char lexeme. Char buffer: %A{xs}, input char: %A{input}."

    member _.DotState inputChar =
        match charBuffer, inputChar with
        | [], None -> ()
        | [ '.' ], None -> ok Lexeme.Dot

        | [], Some _ -> fail "Empty buffer in DotState."
        | [ '.' ], Some '.' ->
            state <- this.InitialState
            ok Lexeme.DoubleDot
            clearCharBuffer ()
        | [ '.' ], Some item ->
            state <- this.InitialState
            reader.Return item
            clearCharBuffer ()
            ok Lexeme.Dot
        | buf, input -> fail $"Unexpected state. Char buffer: %A{buf}, input char: %A{input}"

    member _. GetLexemes () : Result<Lexeme, string> seq =
        seq {
            let mutable inputChar = reader.TryNext ()

            while Option.isSome inputChar do
                state inputChar
                inputChar <- reader.TryNext ()
                match nextLexeme with
                | Some item ->
                    yield item
                    nextLexeme <- None
                | None -> ()

            state None
            match nextLexeme with
            | Some item -> yield item
            | None -> ()

            yield Ok EndOfFile
        }


let charReaderFromTextReader (reader: System.IO.TextReader) =
    let seq = seq {
        let mutable input = reader.Read ()
        while input <> -1 do
            yield (char input)
            input <- reader.Read ()
    }
    new SeqReader<char> (seq)

let charReaderFromStr str =
    new SeqReader<char> (str :> char seq)

let tokenize reader =
    let tokenizer = Tokenizer reader
    tokenizer.GetLexemes ()
