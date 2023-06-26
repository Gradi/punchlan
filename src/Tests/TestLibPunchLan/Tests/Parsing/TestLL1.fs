namespace TestLibPunchLan.Tests.Parsing

open FsUnit
open LibPunchLan.Addons
open LibPunchLan.Lexing
open LibPunchLan.Parsing
open NUnit
open NUnit.Framework
open LibPunchLan.Parsing.LL1
open LibPunchLan.Lexing.Lexer

[<TestFixture>]
module TestLL1 =

    let parseSource str (f: SeqReader<Result<LexemeContainer, string>> -> Result<'b, string>): 'b =
        use reader = charReaderFromStr str
        let lexemes = tokenize reader
        use lexemeReader = new SeqReader<Result<LexemeContainer, string>>(lexemes)
        match f lexemeReader with
        | Error msg ->
            Assert.Fail (sprintf $"Error on input source:\n%s{str}\n%s{msg}")
            failwith ""
        | Ok result -> result

    [<Test>]
    let ``Can parse open directives`` () =
        parseSource "open filename\n" (fun l -> parseOpenDirective l)
        |> should equal [ { OpenDirective.Path = "filename"; Alias = None } ]

        parseSource "open filename\nopen secondFile\n"
                    (fun l -> parseOpenDirective l)
        |> should equal [ { Path = "filename"; Alias = None }
                          { Path = "secondFile"; Alias = None } ]

        parseSource "open lib/std/io as io\nopen lib/rnd\nopen mylib as library\n\n\n"
                    (fun l -> parseOpenDirective l)
        |> should equal [ { Path = "lib/std/io"; Alias = Some "io" }
                          { Path = "lib/rnd"; Alias = None }
                          { Path = "mylib"; Alias = Some "library" } ]

    [<Test>]
    let ``Empty input produces empty directive list`` () =
        parseSource "" (fun l -> parseOpenDirective l)
        |> should equal ([] : OpenDirective list)
