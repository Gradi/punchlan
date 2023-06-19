namespace TestLibPunchLan.Tests.Lexing

open LibPunchLan.Lexing
open NUnit
open NUnit.Framework
open FsUnit
open LibPunchLan.Lexing.Lexer

[<TestFixture>]
module TestReader =

    let runReader str =
        seq {
            let reader = Reader.FromString str
            let mutable char = reader.Next ()

            while Option.isSome char do
                yield char.Value
                char <- reader.Next ()
        } |> List.ofSeq

    let ch r c value = { Char.Char = value; Row = r; Col = c }

    [<Test>]
    let ``Reader reads chars and numbers them correctly`` () =
        let input = "let reader = \nlol here are\n\n   end\n\nl"
        let actual = runReader input
        let expected = [
            (ch 0 0 'l'); (ch 0 1 'e'); (ch 0 2 't'); (ch 0 3 ' '); (ch 0 4 'r'); (ch 0 5 'e'); (ch 0 6 'a'); (ch 0 7 'd'); (ch 0 8 'e'); (ch 0 9 'r'); (ch 0 10 ' '); (ch 0 11 '='); (ch 0 12 ' ')
            (ch 0 13 '\n')

            (ch 1 0 'l'); (ch 1 1 'o'); (ch 1 2 'l'); (ch 1 3 ' '); (ch 1 4 'h'); (ch 1 5 'e'); (ch 1 6 'r'); (ch 1 7 'e'); (ch 1 8 ' '); (ch 1 9 'a'); (ch 1 10 'r'); (ch 1 11 'e')
            (ch 1 12 '\n'); (ch 2 0 '\n')

            (ch 3 0 ' '); (ch 3 1 ' '); (ch 3 2 ' '); (ch 3 3 'e'); (ch 3 4 'n'); (ch 3 5 'd')
            (ch 3 6 '\n'); (ch 4 0 '\n')
            (ch 5 0 'l')
        ]
        actual |> should equal expected

[<TestFixture>]
module TestLexer =

    let runLexer str =
        let reader = Reader.FromString str
        let tokens =
            tokenize reader
            |> List.ofSeq

        let errors =
            tokens
            |> List.where Result.isError
        match errors with
        | [] ->
            tokens
            |> List.map (fun result -> match result with | Ok lexeme -> lexeme | _ -> failwith "Should not happen.")
        | _ ->
            let msgs =
                tokens
                |> List.map (fun result -> match result with | Error msg -> msg | _ -> failwith "Should not happen.")
            let message = String.concat "\n" msgs
            Assert.Fail message
            failwith ""

    let lc y0 x0 y1 x1 lex = { Lexeme = lex; RowStart = y0; ColStart = x0; RowEnd = y1; ColEnd = x1 }

    [<Test>]
    let ``Example 001`` () =
        let input = "func add(a: int32, b: int32) : int32\n    return a + b\nendfunc"
        let actual = runLexer input
        let expected =
            [
                lc 0 0 0 3  (Keyword Func)
                lc 0 5 0 7 (Identifier "add")
                lc 0 8 0 8 LParen
                lc 0 9 0 9 (Identifier "a")
                lc 0 10 0 10 Colon
                lc 0 12 0 16 (Keyword Int32)
                lc 0 17 0 17 Comma
                lc 0 19 0 19 (Identifier "b")
                lc 0 20 0 20 Colon
                lc 0 22 0 26 (Keyword Int32)
                lc 0 27 0 27 RParen
                lc 0 29 0 29 Colon
                lc 0 31 0 35 (Keyword Int32)
                lc 0 36 0 36 Newline
                lc 1 4 1 9 (Keyword Return)
                lc 1 11 1 11 (Identifier "a")
                lc 1 13 1 13 (Operator "+")
                lc 1 15 1 15 (Identifier "b")
                lc 1 16 1 16 Newline
                lc 2 0 2 6 (Keyword EndFunc)
            ]
        actual |> should equal expected
