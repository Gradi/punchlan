
open System.Reflection
open System.Text
open Argu
open System.IO
open LibPunchLan.Addons
open LibPunchLan.Lexing
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking
open LibPunchLan.ResultM
open LibPunchLan
open System.Collections.Generic
open LibPunchLan.CodeGen

type Backend =
    | Nasm

type CommandLineArgs =
    | [<Mandatory; Unique; CustomCommandLine("-f")>] Input of string
    | Backend of Backend
    | [<Mandatory; Unique; CustomCommandLine("-o")>] Output of string

    interface IArgParserTemplate with

        member this.Usage =
            match this with
            | Input _ -> "specifies main(starting) source file"
            | Backend _ -> "specifies target codegen backend. Defaults to Nasm"
            | Output _ -> "specifies output filename"


let visitiedSources = HashSet<string> (System.StringComparer.InvariantCulture)
let parsedSources = List<Source> ()

let getSourceSearchPaths (rootfilepath: string) =
    let asm = Assembly.GetEntryAssembly ()
    if asm = null then failwith "Can't get entry assembly."

    let path = asm.Location
    if System.String.IsNullOrWhiteSpace path then failwith "Can't get entry assembly's location(path)."

    let stdlibdir = Path.Combine(Path.GetDirectoryName path, "stdlibrary")
    let rootfiledir = Path.GetDirectoryName rootfilepath
    if not <| Directory.Exists stdlibdir then [ rootfiledir ]
    else [ rootfiledir; stdlibdir ]

let normPath (path: string) =
    let path =path.Replace ('/', Path.DirectorySeparatorChar)
    sprintf $"%s{path}.pl"

let rec parseSources (filepath: string) (sourceFilename: string) (searchPaths: string list) = result {
    if visitiedSources.Contains filepath then return ()
    else
        if not <| File.Exists filepath then return! resultf $"Source file not found \"%s{filepath}\""
        else
            visitiedSources.Add filepath |> ignore

            use fileStream = File.Open (filepath, FileMode.Open, FileAccess.Read)
            use bufferedStream = new BufferedStream (fileStream)
            use textReader = new StreamReader (bufferedStream, Encoding.UTF8)

            let chars = Lexer.textReaderToCharSeq textReader
            let tokens = Lexer.tokenize (Lexer.indexedChars chars)
            use tokenReader = new SeqReader<Result<LexemeContainer, string>> (tokens)

            let! source = LL1.parseSource sourceFilename tokenReader
            parsedSources.Add source

            for sourceFilename in source.OpenDirectives |> Seq.ofList |> Seq.map (fun od -> od.Path) do
                let normpath = normPath sourceFilename
                let paths = searchPaths |> List.map (fun path -> Path.Combine (path, normpath))

                match paths |> List.tryFind File.Exists with
                | None -> return! resultf $"Source file not found \"%s{normpath}\". Searched for it in %A{paths}"
                | Some path -> do! (parseSources path sourceFilename searchPaths)
}

let printStopwatch (stopwatch: System.Diagnostics.Stopwatch) title =
    let elapsed = stopwatch.Elapsed
    eprintfn $"Spent %O{elapsed} time in.......: %s{title}."

[<EntryPoint>]
let main argv =
    try

        let stopwatch = System.Diagnostics.Stopwatch.StartNew ()
        let commandLine = ArgumentParser.Create<CommandLineArgs>().ParseCommandLine argv
        printStopwatch stopwatch "command line parsing"
        stopwatch.Restart ()

        let rootfilepath = commandLine.GetResult CommandLineArgs.Input
        let sourceFilename = Path.GetFileNameWithoutExtension rootfilepath
        let searchPaths = getSourceSearchPaths rootfilepath
        eprintfn $"Search paths are %A{searchPaths}"
        let parseResult = parseSources rootfilepath sourceFilename searchPaths

        printStopwatch stopwatch (sprintf $"parsing of %d{parsedSources.Count} source files")
        stopwatch.Restart ()

        match parseResult with
        | Error error ->
            eprintfn $"Error parsing source files: \"%s{error}\""
            2
        | Ok () when parsedSources.Count = 0 ->
            failwith "After successfull parsing there are zero(0) sources in collection. Should not happen."
            2
        | Ok () ->
            let program = { Program.Sources = List.ofSeq parsedSources }
            let typeCheckResult = TypeChecker.typeCheckProgram program
            printStopwatch stopwatch "type checking program"
            stopwatch.Restart ()

            match typeCheckResult with
            | [] ->
                match commandLine.GetResult (CommandLineArgs.Backend, Backend.Nasm) with
                | Nasm ->
                    use outputFilestream =
                        match commandLine.GetResult Output with
                        | "-" -> System.Console.OpenStandardOutput ()
                        | name -> File.Open (name, FileMode.Create, FileAccess.Write)
                    use bufferedStream = new BufferedStream (outputFilestream)
                    use textWriter = new StreamWriter (bufferedStream)
                    let codegen: ICodegenerator = NasmCodegenerator.NasmCodegenerator (textWriter, program)
                    codegen.Write ()

                    textWriter.Flush ()
                    printStopwatch stopwatch "code generation"
                    0

            | diags ->
                eprintfn "There are some issues which needed to be solved before compilation:"
                diags
                |> List.iter (fun diag -> eprintfn $"\t%O{diag}")
                2

    with
    | :? ArguParseException as exc ->
        eprintfn $"%s{exc.Message}"
        1
    | exc ->
        eprintfn $"%O{exc}"
        2
