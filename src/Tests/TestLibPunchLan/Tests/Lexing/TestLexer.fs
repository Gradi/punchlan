namespace TestLibPunchLan.Tests.Lexing

open LibPunchLan.Lexing
open NUnit
open NUnit.Framework
open FsUnit
open LibPunchLan.Lexing.Lexer

[<TestFixture>]
module TestLexer =

    let runLexer str =
        use reader = charReaderFromStr str
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

    [<Test>]
    let ``Example 001`` () =
        let input = "func add(a: int32, b: int32) : int32\n    return a + b\nendfunc"
        let actual = runLexer input
        let expected =
            [
                Keyword Keyword.Func
                Identifier "add"
                LParen
                Identifier "a"
                Colon
                Keyword Keyword.Int32
                Comma
                Identifier "b"
                Colon
                Keyword Keyword.Int32
                RParen
                Colon
                Keyword Keyword.Int32
                Newline
                Keyword Keyword.Return
                Identifier "a"
                Operator "+"
                Identifier "b"
                Newline
                Keyword Keyword.EndFunc
                EndOfFile
            ]
        actual |> should equal expected
