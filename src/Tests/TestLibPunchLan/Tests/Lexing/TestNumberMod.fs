namespace TestLibPunchLan.Tests.Lexing

open FsUnit
open LibPunchLan.Lexing
open LibPunchLan.Lexing.NumberMod
open NUnit
open NUnit.Framework

[<TestFixture>]
module ``Tests for NumberMod module`` =

    [<TestCase(null)>]
    [<TestCase("")>]
    [<TestCase("asdalsdkja")>]
    [<TestCase("123bbb")>]
    let ``Returns None if string is incorrect`` (str: string) =
        let actual = tryParseNumber str
        match actual with
        | None -> Assert.Pass ()
        | Some number -> Assert.Fail (sprintf $"Expected \"%s{str}\" to fail to be parsed, but got \"%A{number}\".")

    [<Test>]
    let ``Returns decimal number for decimal input`` () =
        tryParseNumber "1232485923"
        |> should equal (Some <| (Number.Integer [| DecInt.One ; DecInt.Two; DecInt.Three; DecInt.Two; DecInt.Four;
                                                   DecInt.Eight; DecInt.Five; DecInt.Nine; DecInt.Two; DecInt.Three |]))

    [<Test>]
    let ``Returns hex number for hex input`` () =
        tryParseNumber "0xdeadBeef"
        |> should equal (Some <| (Number.HexInteger [| HexInt.D; HexInt.E; HexInt.A; HexInt.D
                                                       HexInt.B; HexInt.E; HexInt.E; HexInt.F; |]))

    [<Test>]
    let ``Returns binary number for binary input`` () =
        tryParseNumber "0b1010000111"
        |> should equal (Some <| (Number.BinaryInteger [| BitInt.One; BitInt.Zero; BitInt.One; BitInt.Zero; BitInt.Zero
                                                          BitInt.Zero; BitInt.Zero; BitInt.One; BitInt.One
                                                          BitInt.One |]))

    [<Test>]
    let ``Returns double for double input`` () =
        tryParseNumber "3.1415"
        |> should equal (Some <| (Number.Double 3.1415 ))
