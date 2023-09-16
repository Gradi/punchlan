namespace TestLibPunchLan.Tests.Lexing

open LibPunchLan.Lexing
open NUnit
open NUnit.Framework
open FsUnit
open LibPunchLan.Lexing.Lexer

[<TestFixture>]
module TestLexer =

    let runLexer str =
        let tokens =
            Lexer.tokenizeFromString str
            |> List.ofSeq

        match tokens |> List.tryFind Result.isError with
        | Some (Error message) ->
            Assert.Fail message
            failwith message
        | Some (Ok _) -> failwith "Should not happen"
        | None ->
            tokens
            |> List.map Result.toOption
            |> List.choose id

    [<Test>]
    let ``Example 001`` () =
        let input = "func add(a: int32, b: int32) : int32\n    return a + b\nendfunc"
        let actual = runLexer input
        let expected =
            [
                { LexemeContainer.Lexeme = Lexeme.Keyword Keyword.Func; Row = 1; Col = 1 }
                { Lexeme = Identifier "add"; Row = 1; Col = 6 }
                { Lexeme = LParen; Row = 1; Col = 9 }
                { Lexeme = Identifier "a"; Row = 1; Col = 10 }
                { Lexeme = Colon; Row = 1; Col = 11 }
                { Lexeme = Keyword Keyword.Int32; Row = 1; Col = 13 }
                { Lexeme = Comma; Row = 1; Col = 18 }
                { Lexeme = Identifier "b"; Row = 1; Col = 20 }
                { Lexeme = Colon; Row = 1; Col = 21 }
                { Lexeme = Keyword Keyword.Int32; Row = 1; Col = 23 }
                { Lexeme = RParen ; Row = 1; Col = 28 }
                { Lexeme = Colon; Row = 1; Col = 30 }
                { Lexeme = Keyword Keyword.Int32; Row = 1; Col = 32 }
                { Lexeme = Newline; Row = 1; Col = 37 }
                { Lexeme = Keyword Keyword.Return; Row = 2; Col = 5 }
                { Lexeme = Identifier "a"; Row = 2; Col = 12 }
                { Lexeme = Operator "+"; Row = 2; Col = 14 }
                { Lexeme = Identifier "b"; Row = 2; Col = 16 }
                { Lexeme = Newline; Row = 2; Col = 17 }
                { Lexeme = Keyword Keyword.EndFunc; Row = 3; Col = 1 }
                { Lexeme = EndOfFile; Row = -1; Col = -1 }
            ]
        actual |> should equivalent expected
