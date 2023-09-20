module LibPunchLan.Parsing.LL1

open LibPunchLan.Addons
open LibPunchLan.Lexing
open LibPunchLan.Parsing.LL1M
open LibPunchLan.ResultM

let rec parseOpenDirectives () = parser {
    let rec parsePath () = parser {
        let! path = readIdentifier

        match! tryConsume (Lexeme.Operator "/") with
        | true ->
            let! rest = parsePath ()
            return sprintf $"%s{path}/%s{rest}"
        | false -> return path
    }

    do! skipNewlines ()
    match! tryConsume (Lexeme.Keyword Keyword.Open) with
    | true ->
        let! path = parsePath ()
        let! alias = parser {
            match! tryConsume (Lexeme.Keyword Keyword.As) with
            | true ->
                let! id = readIdentifier
                return Some id
            | false -> return None
        }

        do! consume Lexeme.Newline
        let! rest = parseOpenDirectives ()
        return { OpenDirective.Path = path; Alias = alias } :: rest

    | false -> return []
}

let parseDotString () = parser {
    match! next with
    | { Lexeme = Lexeme.Identifier id1 } ->
        match! next with
        | { Lexeme = Lexeme.Dot } ->
            let! id2 = readIdentifier
            return { DotString.Name = id2; Alias = Some id1 }
        | input ->
            do! returnLex input
            return { DotString.Name = id1; Alias = None }
    | input ->
        do! returnLex input
        return! parseError input "Expected identifier"
}

let rec parseTypeId () = parser {
    match! next with
    | { Lexeme = Lexeme.Keyword Keyword.Int8 } -> return TypeId.Int8
    | { Lexeme = Lexeme.Keyword Keyword.Uint8 } -> return TypeId.Uint8
    | { Lexeme = Lexeme.Keyword Keyword.Int16 } -> return TypeId.Int16
    | { Lexeme = Lexeme.Keyword Keyword.Uint16 } -> return TypeId.Uint16
    | { Lexeme = Lexeme.Keyword Keyword.Int32 } -> return TypeId.Int32
    | { Lexeme = Lexeme.Keyword Keyword.Uint32 } -> return TypeId.Uint32
    | { Lexeme = Lexeme.Keyword Keyword.Int64 } -> return TypeId.Int64
    | { Lexeme = Lexeme.Keyword Keyword.Uint64 } -> return TypeId.Uint64
    | { Lexeme = Lexeme.Keyword Keyword.Float } -> return TypeId.Float
    | { Lexeme = Lexeme.Keyword Keyword.Double } -> return TypeId.Double
    | { Lexeme = Lexeme.Keyword Keyword.Bool } -> return TypeId.Bool
    | { Lexeme = Lexeme.Keyword Keyword.Char } -> return TypeId.Char
    | { Lexeme = Lexeme.Keyword Keyword.Void } -> return TypeId.Void
    | { Lexeme = Lexeme.Keyword Keyword.Pointer } ->
        do! consume Lexeme.LABracket
        let! typ = parseTypeId ()
        do! consume Lexeme.RABracket
        return TypeId.Pointer typ
    | { Lexeme = Lexeme.Keyword Keyword.Const } ->
        let! typ  = parseTypeId ()
        return TypeId.Const typ
    | { Lexeme = Lexeme.Identifier _ } as input ->
        do! returnLex input
        let! dotStr = parseDotString ()
        return TypeId.Named dotStr
    | input ->
        do! returnLex input
        return! parseError input "Can't parse type id from this"
}

module private rec Expressions =

    let parseExpression () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.Keyword Keyword.Or } ->
                let! right = parseExpr1 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Or (left, right))
            | { Lexeme = Lexeme.Keyword Keyword.And } ->
                let! right = parseExpr1 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.And (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr1 ()
        return! doParse left
    }

    let parseExpr1 () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.LABracket } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Less (left, right))
            | { Lexeme = Lexeme.Operator "<=" } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.LessOrEqual (left, right))
            | { Lexeme = Lexeme.Operator "==" } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Equal (left, right))
            | { Lexeme = Lexeme.RABracket } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Greater (left, right))
            | { Lexeme = Lexeme.Operator ">=" } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.GreaterOrEqual (left, right))
            | { Lexeme = Lexeme.Operator "!=" } ->
                let! right = parseExpr2 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.NotEqual (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr2 ()
        return! doParse left
    }

    let parseExpr2 () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.Keyword Keyword.Xor } ->
                let! right = parseExpr3 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Xor (left, right))
            | { Lexeme = Lexeme.Operator ">>" } ->
                let! right = parseExpr3 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.RShift (left, right))
            | { Lexeme = Lexeme.Operator "<<" } ->
                let! right = parseExpr3 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.LShift (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr3 ()
        return! doParse left
    }

    let parseExpr3 () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.Operator "+" } ->
                let! right = parseExpr4 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Plus (left, right))
            | { Lexeme = Lexeme.Operator "-" } ->
                let! right = parseExpr4 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Minus (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr4 ()
        return! doParse left
    }

    let parseExpr4 () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.Operator "*" } ->
                let! right = parseExpr5 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Multiply (left, right))
            | { Lexeme = Lexeme.Operator "/" } ->
                let! right = parseExpr5 ()
                return! doParse (Expression.BinaryExpression <| BinaryExpression.Division (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr5 ()
        return! doParse left
    }

    let parseExpr5 () = parser {
        let rec doParse left = parser {
            match! next with
            | { Lexeme = Lexeme.Dot } ->
                let! id = readIdentifier
                return! doParse (Expression.MemberAccess (left, id))
            | { Lexeme = Lexeme.LSBracket } ->
                let! right = parseExpression ()
                do! consume Lexeme.RSBracket
                return! doParse (Expression.ArrayAccess (left, right))
            | input ->
                do! returnLex input
                return left
        }

        let! left = parseExpr6 ()
        return! doParse left
    }

    let parseExpr6 () = parser {
        match! next with
        | { Lexeme = Lexeme.Operator "~" } ->
            let! expr = parseExpression ()
            return Expression.Bininversion expr

        | { Lexeme = Lexeme.String str } -> return Expression.Constant (Value.String str)
        | { Lexeme = Lexeme.Number num } -> return Expression.Constant (Value.Number num)
        | { Lexeme = Lexeme.Keyword Keyword.True } -> return Expression.Constant (Value.Boolean true)
        | { Lexeme = Lexeme.Keyword Keyword.False } -> return Expression.Constant (Value.Boolean false)
        | { Lexeme = Lexeme.Char ch } -> return Expression.Constant (Value.Char ch)

        | { Lexeme = Lexeme.Identifier name1 } ->
            match! next with
            | { Lexeme = Lexeme.Dot } as dotLexeme ->
                match! next with
                | { Lexeme = Lexeme.Identifier name2 } as name2Lexeme ->

                    match! next with
                    | { Lexeme = Lexeme.LParen } as input ->
                        do! returnLex input
                        let! funcArguments = parseFunctionArguments ()
                        return Expression.FuncCall ({ DotString.Name = name2; Alias = Some name1 }, funcArguments)
                    | { Lexeme = Lexeme.LCBracket } as input ->
                        do! returnLex input
                        let! fields = parseFieldsInits ()
                        return Expression.StructCreation ({ DotString.Name = name2; Alias = Some name1 }, fields)
                    | input ->
                        do! returnLex input
                        do! returnLex name2Lexeme
                        do! returnLex dotLexeme
                        return Expression.Variable name1

                | input ->
                    do! returnLex input
                    return! parseError input "Expected identifier"

            | { Lexeme = Lexeme.LParen } as input ->
                do! returnLex input
                let! funcArguments = parseFunctionArguments ()
                return Expression.FuncCall ({ DotString.Name = name1; Alias = None }, funcArguments)
            | { Lexeme = Lexeme.LCBracket } as input ->
                do! returnLex input
                let! fields = parseFieldsInits ()
                return Expression.StructCreation ({ DotString.Name = name1; Alias = None }, fields)

            | input ->
                do! returnLex input
                return Expression.Variable name1


        | { Lexeme = Lexeme.Operator "-" } ->
            match! next with
            | { Lexeme = Lexeme.Number num } -> return Expression.Constant (Value.Number (Number.Negative num))
            | input ->
                do! returnLex input
                return! parseError input "Expected constant number after '-'"

        | input ->
            do! returnLex input
            return! parseError input "Expected '~', string, number, true|false, char, identifier.identifier ( ARGS ), identifier.identifier { FIELDS }, identifier, '-' number"
    }

    let parseFunctionArguments () = parser {
        let rec doParse () = parser {
            match! peek with
            | { Lexeme = Lexeme.RParen } -> return []
            | { Lexeme = Lexeme.Comma } ->
                do! consume Lexeme.Comma
                return! doParse ()
            | _ ->
                let! argument = parseExpression ()
                let! rest = doParse()
                return argument :: rest
        }

        do! consume Lexeme.LParen
        let! args = doParse ()
        do! consume Lexeme.RParen
        return args
    }

    let parseFieldsInits () = parser {
        let rec doParse () = parser {
            do! skipNewlines ()
            match! next with
            | { Lexeme = Lexeme.Identifier field } ->
                do! consume Lexeme.Equal
                let! expr = parseExpression ()
                do! consume Lexeme.Newline
                let! rest = doParse ()
                return (field, expr) :: rest
            | { Lexeme = Lexeme.RCBracket } as input ->
                do! returnLex input
                return []
            | input ->
                do! returnLex input
                return! parseError input "Expected identifier or '}'"
        }

        do! consume Lexeme.LCBracket
        let! fields = doParse ()
        do! consume Lexeme.RCBracket
        return fields
    }

    let isExpressionStart (lexeme: Lexeme) =
        match lexeme with
        | Lexeme.Operator "~"
        | Lexeme.String _
        | Lexeme.Number _
        | Lexeme.Char _
        | Lexeme.Keyword Keyword.True
        | Lexeme.Keyword Keyword.False
        | Lexeme.Identifier _
        | Lexeme.Operator "-" -> true
        | _ -> false

module private rec Statements =
    let parseStatements () = parser {
        do! skipNewlines ()
        match! peek with
        | { Lexeme = Lexeme.Keyword Keyword.Var } ->
            let! stat = parseVarStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | { Lexeme = Lexeme.Keyword Keyword.If } ->
            let! stat = parseIfStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | { Lexeme = Lexeme.Keyword Keyword.For } ->
            let! stat = parseForStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | { Lexeme = Lexeme.Keyword Keyword.While } ->
            let! stat = parseWhileStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | { Lexeme = Lexeme.Keyword Keyword.Defer } ->
            let! stat = parseDeferStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | { Lexeme = Lexeme.Keyword Keyword.Return } ->
            let! stat = parseReturnStatement ()
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            return stat :: rest
        | input when Expressions.isExpressionStart input.Lexeme ->
            let! stat = Expressions.parseExpression ()
            let! right = parser {
                match! peek with
                | { Lexeme = Lexeme.Equal } ->
                    do! consume Lexeme.Equal
                    let! right = Expressions.parseExpression ()
                    return Some right
                | _ -> return None
            }
            do! consume Lexeme.Newline
            let! rest = parseStatements ()
            match right with
            | Some right -> return (Statement.VarAssignment (stat, right)) :: rest
            | None -> return (Statement.Expression stat) :: rest
        | _ -> return []
    }

    let parseVarStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.Var)
        let! name = readIdentifier
        do! consume Lexeme.Colon
        let! typeId = parseTypeId ()

        let! initExpr = parser {
            match! peek with
            | { Lexeme = Lexeme.Equal } ->
                do! consume Lexeme.Equal
                let! expr = Expressions.parseExpression ()
                return Some expr
            | _ -> return None
        }

        return Statement.VarDecl (name, typeId, initExpr)
    }

    let parseIfStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.If)
        let! conditionExpr = Expressions.parseExpression ()
        do! consume (Lexeme.Keyword Keyword.Then)
        let! mainBody = parseStatements ()
        let mainIf = { IfCond.Condition = conditionExpr
                       Body = mainBody }

        let rec parseElseIfs () = parser {
            match! peek with
            | { Lexeme = Lexeme.Keyword Keyword.ElseIf } ->
                do! consume (Lexeme.Keyword Keyword.ElseIf)
                let! expr = Expressions.parseExpression ()
                do! consume (Lexeme.Keyword Keyword.Then)
                let! statements = parseStatements ()
                let! rest = parseElseIfs ()
                return { IfCond.Condition = expr
                         Body = statements } :: rest
            | _ -> return []
        }

        let! elseIfs = parseElseIfs ()
        let! elsE = parser {
            match! peek with
            | { Lexeme = Lexeme.Keyword Keyword.Else } ->
                do! consume (Lexeme.Keyword Keyword.Else)
                let! statements = parseStatements ()
                return statements
            | _ -> return []
        }

        do! consume (Lexeme.Keyword Keyword.Fi)

        return Statement.If (mainIf, elseIfs, elsE)
    }

    let parseForStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.For)
        let! varName = readIdentifier
        do! consume (Lexeme.Keyword Keyword.In)
        let! startExpr = Expressions.parseExpression ()
        do! consume Lexeme.DoubleDot
        let! endExpr = Expressions.parseExpression ()

        let! stepExpr = parser {
            match! peek with
            | { Lexeme = Lexeme.DoubleDot } ->
                do! consume Lexeme.DoubleDot
                let! expr = Expressions.parseExpression ()
                return Some expr
            | _ -> return None
        }

        do! consume (Lexeme.Keyword Keyword.Do)
        let! statements = Statements.parseStatements ()
        do! consume (Lexeme.Keyword Keyword.EndFor)

        return Statement.For (varName, startExpr, endExpr, stepExpr, statements)
    }

    let parseWhileStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.While)
        let! condition = Expressions.parseExpression ()
        do! consume (Lexeme.Keyword Keyword.Do)
        let! statements = Statements.parseStatements ()
        do! consume (Lexeme.Keyword Keyword.EndWhile)
        return Statement.While (condition, statements)
    }

    let parseDeferStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.Defer)
        let! body = Statements.parseStatements ()
        do! consume (Lexeme.Keyword Keyword.EndDefer)

        return Statement.Defer body
    }

    let parseReturnStatement () = parser {
        do! consume (Lexeme.Keyword Keyword.Return)

        match! peek with
        | input when Expressions.isExpressionStart input.Lexeme ->
            let! expr = Expressions.parseExpression ()
            return Statement.ReturnExpr expr
        | _ -> return Statement.Return
    }

let parseStructUnionDeclaration () = parser {
    let rec parseFields () = parser {
        do! skipNewlines ()

        match! peek with
        | { Lexeme = Lexeme.Identifier id } ->
            let! id = readIdentifier
            do! consume Lexeme.Colon
            let! typeId = parseTypeId ()
            do! consume Lexeme.Newline
            let! rest = parseFields ()

            return (id, typeId) :: rest
        | _ -> return []
    }

    let! typeType = parser {
        match! next with
        | { Lexeme = Lexeme.Keyword Keyword.Struct } -> return TypeType.Struct
        | { Lexeme = Lexeme.Keyword Keyword.Union } -> return TypeType.Union
        | _ -> return failwith "Should not happen."
    }

    let! name = readIdentifier
    do! consume Lexeme.Newline
    let! fields = parseFields ()
    do! skipNewlines ()

    match typeType with
    | TypeType.Struct -> do! consume (Lexeme.Keyword Keyword.EndStruct)
    | TypeType.Union -> do! consume (Lexeme.Keyword Keyword.EndUnion)

    return { TypeDecl.Name = name
             TypeType = typeType
             Fields = fields }
}

let parseFunctionDeclaration (modifier: Modifier option) = parser {
    let rec parseArguments () = parser {
        match! peek with
        | { Lexeme = Lexeme.Identifier _ } ->
            let! argName = readIdentifier
            do! consume Lexeme.Colon
            let! typeId = parseTypeId ()
            let! rest = parser {
                match! peek with
                | { Lexeme = Lexeme.Comma } ->
                    do! consume Lexeme.Comma
                    return! parseArguments ()
                | { Lexeme = Lexeme.RParen } -> return []
                | input -> return! parseError input "Expected comma or right paren"
            }
            return (argName, typeId) :: rest

        | { Lexeme = Lexeme.RParen } -> return []

        | input -> return! parseError input "Expected an identifier or right paren"
    }

    do! consume (Lexeme.Keyword Keyword.Func)

    let! name = readIdentifier

    do! consume Lexeme.LParen
    let! args = parseArguments ()
    do! consume Lexeme.RParen

    let! returnType = parser {
        match! peek with
        | { Lexeme = Lexeme.Colon } ->
            do! consume Lexeme.Colon
            return! parseTypeId ()
        | _ -> return TypeId.Void
    }

    do! consume Lexeme.Newline
    let! statements = Statements.parseStatements ()
    do! consume (Lexeme.Keyword Keyword.EndFunc)

    return { Function.Name = name
             Args = args
             ReturnType = returnType
             Modifier = modifier
             Body = statements }
}

let parseVariableDeclaration (modifier: Modifier option) = parser {
    do! consume (Lexeme.Keyword Keyword.Var)
    let! varName = readIdentifier
    do! consume Lexeme.Colon
    let! typeId = parseTypeId ()

    let! expr = parser {
        match! peek with
        | { Lexeme = Lexeme.Equal } ->
            do! consume Lexeme.Equal
            let! expr = Expressions.parseExpression ()
            return Some expr
        | _ -> return None
    }

    do! consume Lexeme.Newline

    return { Variable.Name = varName
             TypeId = typeId
             Modifier = modifier
             InitExpr = expr }
}

let rec parseDeclarations () = parser {

    do! skipNewlines ()

    let! modifier = parser {
        match! peek with
        | { Lexeme = Lexeme.Keyword Keyword.Export } ->
            do! consume (Lexeme.Keyword Keyword.Export)
            return Some Modifier.Export
        | { Lexeme = Lexeme.Keyword Keyword.Extern } ->
            do! consume (Lexeme.Keyword Keyword.Extern)
            return Some Modifier.Extern
        | _ -> return None
    }

    match! peek with
    | { Lexeme = Lexeme.Keyword Keyword.Struct }
    | { Lexeme = Lexeme.Keyword Keyword.Union } as input ->
        match modifier with
        | Some modifier -> return! parseError input $"Structs/Unions can't have modifiers (here \"%A{modifier}\")."
        | None ->
            let! typ = parseStructUnionDeclaration ()
            let! rest = parseDeclarations ()
            return (Declaration.Type typ) :: rest

    | { Lexeme = Lexeme.Keyword Keyword.Func } ->
        let! func = parseFunctionDeclaration modifier
        let! rest = parseDeclarations ()
        return (Declaration.Function func) :: rest

    | { Lexeme = Lexeme.Keyword Keyword.Var } ->
        let! var = parseVariableDeclaration modifier
        let! rest = parseDeclarations ()
        return (Declaration.Variable var) :: rest

    | { Lexeme = Lexeme.EndOfFile } as input ->
        match modifier with
        | Some _ -> return! parseError input "Unexpected end of file."
        | None -> return []

    | input -> return! parseError input "Unexpected lexeme at the root of file. Expected struct/union/func/var."
}

let parseSource (filename: string) = parser {
    let! openDirectives = parseOpenDirectives ()
    let! declarations = parseDeclarations ()

    return { Source.OpenDirectives = openDirectives
             Declarations = declarations
             Filename = filename }
}

let parseSourceFromString (input: string) =
    let lexemes = Lexer.tokenizeFromString input
    use reader = new SeqReader<Result<LexemeContainer, string>> (lexemes)

    parseSource "< STRING INPUT >" reader
