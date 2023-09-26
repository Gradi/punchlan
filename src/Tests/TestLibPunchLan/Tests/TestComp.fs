module TestLibPunchLan.Tests.TestComp

open FsUnit
open NUnit.Framework

[<TestCase(0, 2, ExpectedResult = 0)>]
[<TestCase(1, 2, ExpectedResult = 2)>]
[<TestCase(2, 2, ExpectedResult = 2)>]
[<TestCase(3, 2, ExpectedResult = 4)>]
[<TestCase(4, 2, ExpectedResult = 4)>]
[<TestCase(5, 2, ExpectedResult = 6)>]
[<TestCase(5, 8, ExpectedResult = 8)>]
[<TestCase(6, 8, ExpectedResult = 8)>]
[<TestCase(7, 8, ExpectedResult = 8)>]
[<TestCase(8, 8, ExpectedResult = 8)>]
[<TestCase(9, 8, ExpectedResult = 16)>]
[<TestCase(10, 8, ExpectedResult = 16)>]
[<TestCase(11, 8, ExpectedResult = 16)>]
[<TestCase(12, 8, ExpectedResult = 16)>]
[<TestCase(13, 8, ExpectedResult = 16)>]
[<TestCase(14, 8, ExpectedResult = 16)>]
[<TestCase(15, 8, ExpectedResult = 16)>]
[<TestCase(16, 8, ExpectedResult = 16)>]
[<TestCase(17, 8, ExpectedResult = 24)>]
[<TestCase(18, 8, ExpectedResult = 24)>]
let ``Test align returns Ok values`` input align = LibPunchLan.Comp.align input align
