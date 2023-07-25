module LibPunchLan.StringifySyntaxTree

open System.Text
open LibPunchLan.Parsing
open Printf
open LibPunchLan.Lexing.NumberMod


let private writeOpenDirectives (openDirs: OpenDirective list) sb =
    openDirs
    |> List.iter (fun dir ->
        bprintf sb $"open %s{dir.Path}"
        match dir.Alias with
        | Some alias -> bprintf sb $" as %s{alias}"
        | None -> ()

        bprintf sb "\n")

let private writeModifiers (mods: Modifier list) sb =
    let modToStr m =
        match m with
        | Extern -> "extern"
        | Export -> "export"

    match mods with
    | [] -> ()
    | head :: xs ->
        bprintf sb $"%s{modToStr head}"
        xs
        |> List.iter (fun m -> bprintf sb $" %s{modToStr m}")

let private writeDotString (str: DotString) sb =
    let str = String.concat "." str
    bprintf sb $"%s{str}"

let rec private writeTypeId (typ: TypeId) sb =
    match typ with
    | Int8 -> bprintf sb "int8"
    | Uint8 -> bprintf sb "uint8"
    | Int16 -> bprintf sb "int16"
    | Uint16 -> bprintf sb "uint16"
    | Int32 -> bprintf sb "int32"
    | Uint32 -> bprintf sb "uint32"
    | Int64 -> bprintf sb "int64"
    | Uint64 -> bprintf sb "uint64"
    | Float -> bprintf sb "float"
    | Double -> bprintf sb "double"
    | Bool -> bprintf sb "bool"
    | TypeId.Char -> bprintf sb "char"
    | Sizet -> bprintf sb "sizet"
    | Void -> bprintf sb "void"
    | Pointer typ ->
        bprintf sb "pointer<"
        writeTypeId typ sb
        bprintf sb ">"
    | Const typ ->
        bprintf sb "const "
        writeTypeId typ sb
    | Named (name, typeArgs) ->
        writeDotString name sb
        writeTypeArgs typeArgs sb

and private writeTypeArgs (typeArgs: TypeId list) sb =
    match typeArgs with
    | [] -> ()
    | x :: xs ->
        bprintf sb " < "
        writeTypeId x sb
        xs |> List.iter (fun t ->
            bprintf sb ", "
            writeTypeId t sb)
        bprintf sb " >"

let rec private writeExpression (expr: Expression) sb =
    bprintf sb "("
    let writeBinaryExpression (expr: BinaryExpression) =
        match expr with
        | Plus (l, r) ->
            writeExpression l sb
            bprintf sb " + "
            writeExpression r sb
        | Minus (l, r) ->
            writeExpression l sb
            bprintf sb " - "
            writeExpression r sb
        | Multiply (l, r) ->
            writeExpression l sb
            bprintf sb " * "
            writeExpression r sb
        | Division (l, r) ->
            writeExpression l sb
            bprintf sb " / "
            writeExpression r sb
        | Equal (l, r) ->
            writeExpression l sb
            bprintf sb " == "
            writeExpression r sb
        | NotEqual (l, r) ->
            writeExpression l sb
            bprintf sb " != "
            writeExpression r sb
        | Less (l, r) ->
            writeExpression l sb
            bprintf sb " < "
            writeExpression r sb
        | LessOrEqual (l, r) ->
            writeExpression l sb
            bprintf sb " <= "
            writeExpression r sb
        | Greater (l, r) ->
            writeExpression l sb
            bprintf sb " > "
            writeExpression r sb
        | GreaterOrEqual (l, r) ->
            writeExpression l sb
            bprintf sb " >= "
            writeExpression r sb
        | Or (l, r) ->
            writeExpression l sb
            bprintf sb " | "
            writeExpression r sb
        | And (l, r) ->
            writeExpression l sb
            bprintf sb " & "
            writeExpression r sb
        | Xor (l, r) ->
            writeExpression l sb
            bprintf sb " xor "
            writeExpression r sb
        | RShift (l, r) ->
            writeExpression l sb
            bprintf sb " >> "
            writeExpression r sb
        | LShift (l, r) ->
            writeExpression l sb
            bprintf sb " << "
            writeExpression r sb


    match expr with
    | Constant (String str) -> bprintf sb $"\"%s{str}\""
    | Constant (Number number) -> bprintf sb $"%s{numToStr number}"
    | Constant (Boolean bool) -> bprintf sb $"%b{bool}"
    | Constant (Char ch) -> bprintf sb $"%c{ch}"
    | Variable name -> bprintf sb $"%s{name}"
    | FuncCall (name, typeArgs, exprs) ->
        bprintf sb $"%s{name}"
        writeTypeArgs typeArgs sb
        bprintf sb " ("
        match exprs with
        | [] -> ()
        | head :: xs ->
            writeExpression head sb
            xs |> List.iter (fun e ->
                bprintf sb ", "
                writeExpression e sb)
        bprintf sb ")"
    | MemberCall (this, name, typeArgs, funcArgs) ->
        writeExpression this sb
        bprintf sb $".%s{name}"
        writeTypeArgs typeArgs sb
        bprintf sb " ("
        match funcArgs with
        | [] -> ()
        | x :: xs ->
            writeExpression x sb
            xs |> List.iter (fun e ->
                bprintf sb ", "
                writeExpression e sb)
        bprintf sb ")"
    | MemberAccess (expr, name) ->
        writeExpression expr sb
        bprintf sb $".%s{name}"
    | BinaryExpression binExpr -> writeBinaryExpression binExpr
    | ArrayAccess (expr, index) ->
        writeExpression expr sb
        bprintf sb "["
        writeExpression index sb
        bprintf sb "]"
    | StructCreation (name, typeArgs, fields) ->
        writeDotString name sb
        writeTypeArgs typeArgs sb
        bprintf sb " {\n"
        match fields with
        | [] -> ()
        | (f0, e0) :: xs ->
            bprintf sb $"%s{f0} = "
            writeExpression e0 sb
            xs |> List.iter (fun (f, e) ->
                bprintf sb "\n"
                bprintf sb $"%s{f} = "
                writeExpression e sb)
        bprintf sb "\n}"
    | AddressOf name ->
        bprintf sb $"& %s{name}"
    | Bininversion expr ->
        bprintf sb "~"
        writeExpression expr sb

    bprintf sb ")"

let rec private writeStatements (stats: Statement list) sb =
    let writeStat (stat: Statement) =
        match stat with

        | VarDecl (name, typ, expr) ->
            bprintf sb $"var %s{name} : "
            writeTypeId typ sb
            match expr with
            | Some expr ->
                bprintf sb " = "
                writeExpression expr sb
            | None -> ()

        | VarAssignment (name, expr) ->
            writeDotString name sb
            writeExpression expr sb

        | Statement.FuncCall (name, typeArgs, expr) ->
            writeDotString name sb
            match typeArgs with
            | [] -> ()
            | x :: xs ->
                bprintf sb " <"
                writeTypeId x sb
                xs |> List.iter (fun t ->
                    bprintf sb ", "
                    writeTypeId t sb)
                bprintf sb ">"

            match expr with
            | [] -> bprintf sb " ()"
            | x :: xs ->
                bprintf sb " ("
                writeExpression x sb
                xs |> List.iter (fun e ->
                    bprintf sb ", "
                    writeExpression e sb)
                bprintf sb ")"

        | If (ifCond, elseIf, elsE) ->
            bprintf sb "if "
            writeExpression ifCond.Condition sb
            bprintf sb " then\n"
            writeStatements ifCond.Body sb

            elseIf |> List.iter(fun ifCond ->
                bprintf sb "elseif "
                writeExpression ifCond.Condition sb
                bprintf sb " then\n"
                writeStatements ifCond.Body sb)

            match elsE with
            | [] -> ()
            | xs ->
                bprintf sb "else\n"
                writeStatements xs sb
            bprintf sb "fi"

        | For (varName, initExpr, endExpr, stepExpr, stats) ->
            bprintf sb $"for %s{varName} .. "
            writeExpression initExpr sb
            bprintf sb " .. "
            writeExpression endExpr sb
            stepExpr |> Option.iter (fun expr ->
                bprintf sb " .. "
                writeExpression expr sb)
            bprintf sb " do\n"
            writeStatements stats sb
            bprintf sb "endfor"

        | While (expr, stats) ->
            bprintf sb "while "
            writeExpression expr sb
            bprintf sb " do\n"
            writeStatements stats sb
            bprintf sb "endwhile"

        | Defer stats ->
            bprintf sb "defer\n"
            writeStatements stats sb
            bprintf sb "enddefer"

        | Return -> bprintf sb "return"

        | ReturnExpr expr ->
            bprintf sb "return "
            writeExpression expr sb

        | ArrayAssignment (name, expr) ->
            writeDotString name sb
            bprintf sb " ["
            writeExpression expr sb
            bprintf sb "]"


    stats |> List.iter (fun s ->
        writeStat s
        bprintf sb "\n")

let private writeVariable (variable: Variable) sb =
    writeModifiers variable.Modifiers sb
    bprintf sb $"var %s{variable.Name} : "
    writeTypeId variable.TypeId sb
    match variable.InitExpr with
    | Some expr ->
        bprintf sb" = "
        writeExpression expr sb
    | None -> ()
    bprintf sb "\n"

let private writeFunction (func: Function) sb =
    writeModifiers func.Modifiers sb
    bprintf sb $"func %s{func.Name}"
    match func.TypeArgs with
    | [] -> ()
    | _ ->
        let typeArgs = String.concat ", " func.TypeArgs
        bprintf sb $" < %s{typeArgs} >"

    bprintf sb " ("
    match func.Args with
    | [] -> ()
    | (name, typeId) :: xs ->
        bprintf sb $"%s{name} : "
        writeTypeId typeId sb
        xs
        |> List.iter (fun (name, typeId) ->
            bprintf sb $", %s{name} : "
            writeTypeId typeId sb)
    bprintf sb ")"

    match func.ReturnType with
    | TypeId.Void -> ()
    | typeId ->
        bprintf sb " : "
        writeTypeId typeId sb
    bprintf sb "\n"
    writeStatements func.Body sb
    bprintf sb "endfunc\n"

let writeType (typ: TypeDecl) sb =
    match typ.TypeType with
    | TypeType.Struct -> bprintf sb "struct"
    | TypeType.Union -> bprintf sb "union"
    | TypeType.Enum -> bprintf sb "enum"

    bprintf sb $" %s{typ.Name}"

    match typ.TypeArgs with
    | [] -> ()
    | xs ->
        let typeArgs = String.concat ", " typ.TypeArgs
        bprintf sb $" <%s{typeArgs}>"
    bprintf sb "\n"

    typ.Fields
    |> List.iter (fun (name, typeId) ->
        bprintf sb $"%s{name} : "
        writeTypeId typeId sb
        bprintf sb "\n")

    typ.Functions
    |> List.iter (fun func -> writeFunction func sb)

    match typ.TypeType with
    | TypeType.Struct -> bprintf sb "endstruct"
    | TypeType.Union -> bprintf sb "endunion"
    | TypeType.Enum -> bprintf sb "endenum"
    bprintf sb "\n"

let private writeDeclarations (decls: Declaration list) sb =
    let writeDecl decl =
        match decl with
        | Declaration.Variable variable -> writeVariable variable sb
        | Declaration.Function functioN -> writeFunction functioN sb
        | Declaration.Type typ -> writeType typ sb

    match decls with
    | [] -> ()
    | head :: xs ->
        writeDecl head
        xs |> List.iter (fun d ->
            bprintf sb "\n"
            writeDecl d)


let stringify (source: Source) =
    let sb = StringBuilder ()

    writeOpenDirectives source.OpenDirectives sb
    writeDeclarations source.Declarations sb
    sb.ToString ()
