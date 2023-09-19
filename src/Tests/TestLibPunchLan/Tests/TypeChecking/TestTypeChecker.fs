module TestLibPunchLan.Tests.TypeChecking.TestTypeChecker

open System
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking

let inputSources =
    let asm = Assembly.GetExecutingAssembly ()

    asm.GetManifestResourceNames ()
    |> Array.filter (fun t -> Regex.IsMatch (t, "TypeChecking\\.CodeSamples\\.Sample_[0-9]+\\.in"))
    |> Array.map (fun name ->
        use stream = asm.GetManifestResourceStream name
        use textReader = new System.IO.StreamReader (stream, Encoding.UTF8)
        textReader.ReadToEnd ())

[<Test>]
let ``All code samples produces zero errors(warnings)`` () =
    Assert.That(Array.length inputSources, Is.Not.Zero)

    for inputSource in inputSources do
        match LL1.parseSourceFromString inputSource with
        | Error error -> Assert.Fail error
        | Ok source ->
            let program = { Program.Sources = [ source ] }

            match TypeChecker.typeCheckProgram program with
            | [] -> ()
            | diags ->
                let diags =
                    diags |> List.map (fun d -> d.ToString ())
                let diags = String.concat "\n" diags
                Assert.Fail diags
