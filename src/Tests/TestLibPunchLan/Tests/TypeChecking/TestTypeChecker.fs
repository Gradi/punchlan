module TestLibPunchLan.Tests.TypeChecking.TestTypeChecker

open System
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking

type ShouldCase =
    | Success
    | Fail of string

type InputSource =
    { Case: ShouldCase
      Name: string
      Text: string }

let inputSources =
    let asm = Assembly.GetExecutingAssembly ()

    asm.GetManifestResourceNames ()
    |> Array.filter (fun t -> Regex.IsMatch (t, "TypeChecking\\.CodeSamples\\.Sample_[0-9]+\\.in"))
    |> Array.map (fun name ->
        use stream = asm.GetManifestResourceStream name
        use textReader = new System.IO.StreamReader (stream, Encoding.UTF8)

        let allText = textReader.ReadToEnd ()
        use textReader = new System.IO.StringReader (allText)
        let firstLine = textReader.ReadLine ()
        let case =
            match firstLine.StartsWith "#FAIL " with
            | true -> ShouldCase.Fail <| (firstLine.Substring 6).TrimEnd ()
            | false -> ShouldCase.Success

        let text =
            match case with
            | Fail _ -> textReader.ReadToEnd ()
            | Success -> allText

        { Case = case; Name = name; Text = text })

[<Test>]
let ``All code samples produces zero errors(warnings)`` () =
    Assert.That(Array.length inputSources, Is.Not.Zero)

    for inputSource in inputSources do
        match LL1.parseSourceFromString inputSource.Text with
        | Error error -> Assert.Fail (sprintf $"Can't parse %s{inputSource.Name}: %s{error}")
        | Ok source ->
            let program = { Program.Sources = [ source ] }

            match TypeChecker.typeCheckProgram program, inputSource.Case with
            | [], Success -> ()
            | [], Fail pattern -> Assert.Fail (sprintf $"%s{inputSource.Name} should have failed with pattern \"%s{pattern}\"")
            | diags, shouldcase ->
                let diags =
                    diags |> List.map (fun d -> d.ToString ())
                let diags = String.concat "\n" diags

                match shouldcase with
                | Success -> Assert.Fail (sprintf $"%s{inputSource.Name} should have succeeded, but it failed %s{diags}")
                | Fail pattern ->
                    match Regex.IsMatch (diags, pattern) with
                    | true -> ()
                    | false -> Assert.Fail (sprintf $"%s{inputSource.Name} failed, but pattern doesn't match. %s{diags}")
