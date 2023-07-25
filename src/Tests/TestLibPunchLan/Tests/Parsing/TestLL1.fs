module TestLibPunchLan.Tests.Parsing.TestLL1

open System.Text
open LibPunchLan
open LibPunchLan.Parsing
open System.Reflection
open System.Text.RegularExpressions
open System.IO
open FsUnit
open NUnit.Framework
open NUnit
open LibPunchLan.Parsing

type CodeSample =
    { InputName: string
      Input: string
      Expected: string }

let codeSamples =
    let assembly = Assembly.GetAssembly typeof<CodeSample>.DeclaringType
    let inputSamples =
        assembly.GetManifestResourceNames ()
        |> Array.filter (fun name -> Regex.IsMatch (name, "Sample_[0-9]+\.in"))

    inputSamples
    |> Array.map (fun inputSampleName ->
        let outputSampleName = inputSampleName.Replace (".in", ".out")

        use inputStream = assembly.GetManifestResourceStream inputSampleName
        if inputStream = null then failwithf $"Can't read '%s{inputSampleName}'."
        use textReader = new StreamReader (inputStream, Encoding.UTF8)
        let inputSample = textReader.ReadToEnd ()

        use outStream = assembly.GetManifestResourceStream outputSampleName
        if outStream = null then failwithf $"Can't read '$s{outputSampleName}'."
        use textReader = new StreamReader (outStream, Encoding.UTF8)
        let outputSample = textReader.ReadToEnd().Replace("\r", "")

        { InputName = inputSampleName; Input = inputSample; Expected = outputSample })


[<Test>]
let ``Run all samples`` () =
    for codeSample in codeSamples do
        match LL1.parseSourceFromStr codeSample.Input with
        | Error err -> Assert.Fail (sprintf $"Error parsing '%s{codeSample.InputName}': %s{err}")
        | Ok source ->
            let actual = StringifySyntaxTree.stringify source
            if actual <> codeSample.Expected then
                fprintfn TestContext.Out $"Input sample: '%s{codeSample.InputName}'"
                fprintfn TestContext.Out $"Expected:\n%s{codeSample.Expected}\n\n\n"
                fprintfn TestContext.Out $"Actual:\n%s{actual}\n\n\n"
                actual |> should equal codeSample.Expected
