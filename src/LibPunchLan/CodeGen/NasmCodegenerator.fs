module LibPunchLan.CodeGen.NasmCodegenerator

open System.Text
open LibPunchLan
open LibPunchLan.Comp
open LibPunchLan.Parsing
open LibPunchLan.TypeChecking
open LibPunchLan.TypeChecking.TypeChecker
open LibPunchLan.TypeChecking.TypeCheckerM
open LibPunchLan.Parsing
open LibPunchLan.Lexing
open System.IO
open FSharp.Reflection

let getLabel (name: string) (source: Source) =
    let filename = source.Filename.Replace ('/', '_')
    sprintf $"%s{filename}___%s{name}"

let bytes2hex (bytes: byte array) =
    bytes
    |> Array.map (fun b ->
        let b = b.ToString "x"
        sprintf $"%s{b}h")


type NasmCodegenerator (tw: TextWriter, program: Program) =

    let fprintf text = Printf.fprintf tw text
    let fprintfn text = Printf.fprintfn tw text

    let writeString (str: string) =
        let bytes = Encoding.UTF8.GetBytes str
        let hexChars = bytes2hex bytes
        let safeStr = str.Replace("\n", "\\n").Replace("\r", "\\r")
        fprintf "db "
        hexChars |> Array.iter (fun c -> fprintf $"%s{c},")
        fprintfn $"00h, 00h, 00h, 00h, 00h, 00h, 00h, 00h ;; \"%s{safeStr}\""

    let writeBoolean (value: bool) =
        match value with
        | true -> fprintfn  $"dq ffffffffffffffffh ;; Boolean '%b{value}'"
        | false -> fprintfn $"dq 0000000000000000h ;; Boolean '%b{value}'"

    let writeNumber (num: Number) (byteSize: int) =
        let sign = NumberMod.getNumberSign num
        let num = NumberMod.unwrapNegative num

        match byteSize with
        | 1 -> fprintf "db "
        | 2 -> fprintf "dw "
        | 4 -> fprintf "dd "
        | 8 -> fprintf "dq "
        | _ -> fprintf "dq "

        if sign < 0 then fprintf "-"

        match num with
        | Number.Integer decimals ->
            decimals |> Array.iter (fun d -> fprintf $"%s{NumberMod.decIntToStr d}")
        | Number.HexInteger hexes ->
            hexes |> Array.iter (fun h -> fprintf $"$s{NumberMod.hexIntToStr h}")
            fprintf "h"
        | Number.BinaryInteger bits ->
            bits |> Array.iter (fun b -> fprintf $"%s{NumberMod.bitIntToStr b}")
            fprintf "b"
        | Number.Double dbl -> fprintf $"%f{dbl}"
        | Negative _ -> failwith "Negative numbers should be stripped at this point. Should not happen."
        fprintfn ""

    let writeVariable (var: Variable) (source: Source) reserve =
        let rec writeExpr expr =
            match expr with
            | Expression.Constant (Value.String str) -> writeString str

            | Expression.Constant (Value.Boolean value) -> writeBoolean value

            | Expression.Constant (Value.Char char) ->
                let charStr = (byte char).ToString "x"
                fprintfn $"%s{charStr}h ;; Char '%c{char}'"

            | Expression.Constant (Value.Number number) ->
                let size = getTypeIdSize var.TypeId |> runtchecker source program
                writeNumber number size

            | Expression.StructCreation (name, fieldsInits) ->
                let typeDecl = locateTypeDecl name |> runtchecker source program
                fprintfn $";;; %A{typeDecl.TypeType} initialization: \"%O{name}\" "
                match typeDecl.TypeType with
                | TypeType.Struct ->

                    for fieldName, fieldType in typeDecl.Fields do
                        let fieldSize = getTypeIdSize fieldType |> runtchecker source program
                        fprintfn $";; Field \"%s{fieldName}\""
                        match MList.tryLookup fieldName fieldsInits with
                        | None -> reserve fieldSize
                        | Some expr -> writeExpr expr
                        reserve ((align fieldSize 8) - fieldSize)

                | TypeType.Union ->
                    let fieldName, expr = List.head fieldsInits
                    match MList.tryLookup fieldName typeDecl.Fields with
                    | Some typ ->
                        let fieldSize = getTypeIdSize typ |> runtchecker source program
                        fprintfn $"Field \"%s{fieldName}\""
                        writeExpr expr
                        reserve ((align fieldSize 8) - fieldSize)
                    | None -> failwithf $"Type \%O{name}\" doesn't have field named \"%s{fieldName}\". Should not happen."

            | expr ->
                let unionCaseInfo, _ = FSharpValue.GetUnionFields (expr, expr.GetType())
                failwithf $"Expression \"%s{unionCaseInfo.Name}\" is not supported. Should not happen."

        let label = getLabel var.Name source
        match var.Modifier with
        | None -> fprintfn $"static %s{label}"
        | Some Modifier.Export -> printfn $"global %s{label}"
        | Some Modifier.Extern -> printfn $"extern %s{label}"

        fprintfn $"%s{label}:"
        match var.InitExpr with
        | None -> reserve (getTypeIdSize var.TypeId |> runtchecker source program)
        | Some expr -> writeExpr expr

    let writeVariables (source: Source) =
        let vars = source |> getVariableDeclarations
        let rdataVars = vars |> List.filter (fun v -> TypeId.isConstType v.TypeId)
        let bssVars = vars |> List.filter (fun v -> (not <| TypeId.isConstType v.TypeId) && Option.isNone v.InitExpr)
        let dataVars = vars |> List.filter (fun v -> not <| TypeId.isConstType v.TypeId && Option.isSome v.InitExpr)
        assert (List.length vars = (List.length rdataVars + List.length bssVars + List.length dataVars))

        fprintfn ";;; Readonly variables"
        fprintfn "section .rdata align=8"
        rdataVars |> List.iter (fun v ->
            writeVariable v source (fun s -> fprintfn $"times %d{s} db 0")
            fprintfn "align 8, db 0")
        fprintf "\n"

        fprintfn ";;; Uninitialized variables"
        fprintfn "section .bss align=8"
        bssVars |> List.iter (fun v ->
            writeVariable v source (fun s -> fprintfn $"resb %d{s}")
            fprintfn "align 8, resb 1")
        fprintf "\n"

        fprintfn ";;; Normal read & write variables"
        fprintfn "section .data align=8"
        dataVars |> List.iter (fun v ->
            writeVariable v source (fun s -> fprintfn $"times %d{s} db 0")
            fprintfn "align 8, db 0")

        fprintf "\n"



    let writeSource (source: Source) =
        fprintfn "bits 64"
        fprintfn $";;; Start of.....%s{source.Filename}\n"
        writeVariables source
        fprintfn $";;; End of.......%s{source.Filename}\n"

    interface ICodegenerator with

        member _.Write() =
            for source in program.Sources do
                writeSource source
