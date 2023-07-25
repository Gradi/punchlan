module LibPunchLan.Parsing.LL1

open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open LibPunchLan.Addons
open LibPunchLan.ResultM
open LibPunchLan.Lexing

type private Reader = Result<Lexeme, string> SeqReader

[<AbstractClass; Sealed>]
type private Help() =

    static member Error (lexeme: Lexeme,
                           msg: string,
                           [<CallerMemberName; DefaultParameterValue(""); Optional>] memberName: string) =
        Error (sprintf $"Parse error at \"%s{memberName}\", current lexeme: \"%A{lexeme}\" Msg: \"%s{msg}\".")

    static member ConsumeLexeme (lexeme: Lexeme,
                                 reader: Reader,
                                 [<CallerMemberName; DefaultParameterValue(""); Optional>] memberName: string) =
        result {
            match! reader.Next () with
            | actual when actual = lexeme -> return ()
            | actual -> return! resultf $"%s{memberName}: Expected a '%A{lexeme}' lexeme, but found '%A{actual}'."
        }

    static member UnexpectedLexeme (lexeme: Lexeme,
                                    [<CallerMemberName; DefaultParameterValue(""); Optional>] memberName: string) =
        Help.Error (lexeme, "Unexpected lexeme", memberName)

let rec private skipNewlines (reader: Reader) = result {
    match! reader.Next () with
    | Newline -> do! skipNewlines reader
    | input ->
        reader.Return (Ok input)
        return ()
}

let private parseDotString (reader: Reader) : Result<DotString, string> = result {
    let rec doParse () = result {
        match! reader.Next () with
        | Identifier id ->
            let! rest = doParse ()
            return id :: rest
        | Dot ->
            return! doParse ()
        | input ->
            reader.Return (Ok input)
            return []
    }

    match! doParse () with
    | [] -> return! resultf "Failed to parse string of 'id0.id1.id2...idN'."
    | xs -> return xs
}

module private rec TypeIds =
    let parseTypeId (reader: Reader) = result {
        match! reader.Next () with
        | Keyword Keyword.Int8  -> return TypeId.Int8
        | Keyword Keyword.Uint8 -> return TypeId.Uint8
        | Keyword Keyword.Int16 -> return TypeId.Int16
        | Keyword Keyword.Uint16 -> return TypeId.Uint16
        | Keyword Keyword.Int32 -> return TypeId.Int32
        | Keyword Keyword.Uint32 -> return TypeId.Uint32
        | Keyword Keyword.Double -> return TypeId.Double
        | Keyword Keyword.Float -> return TypeId.Float
        | Keyword Keyword.Char -> return TypeId.Char
        | Keyword Keyword.Bool -> return TypeId.Bool
        | Keyword Keyword.Sizet -> return TypeId.Sizet
        | Keyword Keyword.Void -> return TypeId.Void
        | Keyword Pointer ->
            do! Help.ConsumeLexeme (Lexeme.LABracket, reader)
            let! pointedType = parseTypeId reader
            do! Help.ConsumeLexeme (Lexeme.RABracket, reader)
            return TypeId.Pointer pointedType

        | Keyword Keyword.Const  ->
            let! typeId = parseTypeId reader
            return TypeId.Const typeId

        | Identifier id  ->
            reader.Return (Ok (Identifier id))
            let! dotString = parseDotString reader
            let! typeArguments = parseTypeArguments reader
            return TypeId.Named (dotString, typeArguments)

        | input -> return! Help.Error (input, "Unexpected lexeme")
    }

    let parseTypeArguments (reader: Reader) =
        let rec doParse () = result {
            let! typeId = parseTypeId reader
            match! reader.Next () with
            | Comma ->
                let! rest = doParse ()
                return typeId :: rest
            | input ->
                reader.Return (Ok input)
                return [ typeId ]
        }

        result {
            match! reader.Next () with
            | LABracket ->
                let! typeArguments = doParse ()
                do! Help.ConsumeLexeme (RABracket, reader)
                return typeArguments
            | input ->
                reader.Return (Ok input)
                return []
        }

    let parseTypeArgumentNames (reader: Reader) =
        let rec doParse () = result {
            match! reader.Next () with
            | Identifier id ->

                match! reader.Next () with
                | Comma ->
                    let! rest = doParse ()
                    return id :: rest
                | input ->
                    reader.Return (Ok input)
                    return [ id ]
            | input -> return! Help.UnexpectedLexeme input
        }

        result {
            match! reader.Next () with
            | LABracket ->
                let! argsNames = doParse ()
                do! Help.ConsumeLexeme (RABracket, reader)
                return argsNames
            | input ->
                reader.Return (Ok input)
                return []
        }

module private rec Expressions =

    let parseExpression (reader: Reader) = result {
        let! expr = pExpr1 reader

        match! reader.Next () with
        | Operator "<" ->
            let! rexpr = pExpr1 reader
            return Expression.BinaryExpression (BinaryExpression.Less (expr, rexpr))
        | LessThanOrEqual ->
            let! rexpr = pExpr1 reader
            return Expression.BinaryExpression (BinaryExpression.LessOrEqual (expr, rexpr))
        | DEqual ->
            let! rexpr = pExpr1 reader
            return Expression.BinaryExpression (BinaryExpression.Equal (expr, rexpr))
        | Operator ">" ->
            let! rexpr = pExpr1 reader
            return Expression.BinaryExpression (BinaryExpression.Greater (expr, rexpr))
        | GreaterThanOrEqual ->
            let! rexpr = pExpr1 reader
            return Expression.BinaryExpression (BinaryExpression.GreaterOrEqual (expr, rexpr))
        | input ->
            reader.Return (Ok input)
            return expr
    }

    let pExpr1 (reader: Reader) = result {
        let rec doParse expr = result {
            match! reader.Next () with
            | Operator "+" ->
                let! rexpr = pExpr2 reader
                return! doParse (Expression.BinaryExpression (BinaryExpression.Plus (expr, rexpr)))
            | Operator "-" ->
                let! rexpr = pExpr2 reader
                return! doParse (Expression.BinaryExpression (BinaryExpression.Minus (expr, rexpr)))
            | input ->
                reader.Return (Ok input)
                return expr
        }
        let! expr = pExpr2 reader
        return! doParse expr
    }

    let pExpr2 (reader: Reader) = result {
        let rec doParse expr = result {
            match! reader.Next () with
            | Operator "*" ->
                let! rexpr = pExpr3 reader
                return! doParse (Expression.BinaryExpression (BinaryExpression.Multiply (expr, rexpr)))
            | Operator "/" ->
                let! rexpr = pExpr3 reader
                return! doParse (Expression.BinaryExpression (BinaryExpression.Division (expr, rexpr)))
            | input ->
                reader.Return (Ok input)
                return expr
        }
        let! expr = pExpr3 reader
        return! doParse expr
    }

    let pExpr3 (reader: Reader) = result {
        let! expr = pExpr4 reader

        match! reader.Next () with
        | Operator "|" ->
            let! rexpr = pExpr4 reader
            return Expression.BinaryExpression (BinaryExpression.Or (expr, rexpr))
        | Operator "&" ->
            let! rexpr = pExpr4 reader
            return Expression.BinaryExpression (BinaryExpression.And (expr, rexpr))
        | Keyword Keyword.Xor ->
            let! rexpr = pExpr4 reader
            return Expression.BinaryExpression (BinaryExpression.Xor (expr, rexpr))
        | input ->
            reader.Return (Ok input)
            return expr
    }

    let pExpr4 (reader: Reader) = result {
        let rec doParse expr = result {
            match! reader.Next () with
            | Dot ->

                match! reader.Next () with
                | Identifier id ->
                    let! typeArgs = TypeIds.parseTypeArguments reader

                    match! reader.Next () with
                    | LParen ->
                        reader.Return (Ok LParen)
                        let! funcArgs = parseFunctionArgs reader
                        return! doParse (Expression.MemberCall(expr, id, typeArgs, funcArgs))
                    | LSBracket ->
                        let array = Expression.MemberAccess (expr, id)
                        let! arrayIndex = pExpr5 reader
                        do! Help.ConsumeLexeme (RSBracket, reader)
                        return! doParse (Expression.ArrayAccess(array, arrayIndex))
                    | LCBracket ->
                        let! fields = parseFieldsInits reader
                        do! Help.ConsumeLexeme (RCBracket, reader)

                        match expr with
                        | Variable id0 -> return Expression.StructCreation ([id0; id], typeArgs, fields)
                        | expr -> return! resultf $"Invalid syntax: Struct creation with the left node being %s{expr.GetType().ToString()}."

                    | input ->
                        reader.Return (Ok input)
                        return! doParse (Expression.MemberAccess (expr, id))
                | input -> return! Help.UnexpectedLexeme input

            | input ->
                reader.Return (Ok input)
                return expr
        }
        let! expr = pExpr5 reader
        return! doParse expr
    }

    let pExpr5 (reader: Reader) = result {
        match! reader.Next () with
        | Lexeme.String str -> return Expression.Constant (Value.String str)
        | Lexeme.Char ch -> return Expression.Constant (Value.Char ch)
        | Lexeme.Number num -> return Expression.Constant (Value.Number num)
        | Keyword Keyword.True -> return Expression.Constant (Value.Boolean true)
        | Keyword Keyword.False -> return Expression.Constant (Value.Boolean false)

        | Identifier id ->
            let! typeArgs = TypeIds.parseTypeArguments reader

            match! reader.Next () with
            | LParen ->
                reader.Return (Ok LParen)
                let! funcArgs = parseFunctionArgs reader
                return Expression.FuncCall (id, typeArgs, funcArgs)
            | LSBracket ->
                let! expr = parseExpression reader
                do! Help.ConsumeLexeme (RSBracket, reader)
                return Expression.ArrayAccess (Expression.Variable id, expr)
            | LCBracket ->
                let! fields = parseFieldsInits reader
                do! Help.ConsumeLexeme (RCBracket, reader)
                return Expression.StructCreation ([id], typeArgs, fields)
            | input ->
                reader.Return (Ok input)
                return Expression.Variable id

        | LParen ->
            let! expr = parseExpression reader
            do! Help.ConsumeLexeme (RParen, reader)
            return expr
        | Operator "~" ->
            let! expr = parseExpression reader
            return Expression.Bininversion expr
        | Operator "&" ->
            match! reader.Next () with
            | Identifier id -> return Expression.AddressOf id
            | input -> return! Help.UnexpectedLexeme input
        | input -> return! Help.UnexpectedLexeme input
    }

    let parseFunctionArgs (reader: Reader) = result {
        let rec doParse () = result {
            match! reader.Peek () with
            | RParen -> return []
            | _ ->
                let! expr = parseExpression reader
                match! reader.Next () with
                | Comma ->
                    let! rest = doParse ()
                    return expr :: rest
                | input ->
                    reader.Return (Ok input)
                    return [ expr ]
        }
        do! Help.ConsumeLexeme (LParen, reader)
        let! args = doParse ()
        do! Help.ConsumeLexeme (RParen, reader)
        return args
    }

    let parseFieldsInits (reader: Reader) = result {
        do! skipNewlines reader
        match! reader.Next () with
        | Identifier id ->
            do! Help.ConsumeLexeme (Equal, reader)
            let! expr = parseExpression reader
            do! Help.ConsumeLexeme (Newline, reader)
            let! rest = parseFieldsInits reader
            return (id, expr) :: rest
        | RCBracket ->
            reader.Return (Ok RCBracket)
            return []
        | input -> return! Help.UnexpectedLexeme input
    }

module private rec Statements =
    let parseStatements isEndOfStats (reader: Reader) = result {
        do! skipNewlines reader
        let! input = reader.Peek ()
        if isEndOfStats input then return []
        else

            let! statement = parseStatement reader
            do! Help.ConsumeLexeme (Newline, reader)
            do! skipNewlines reader

            let! input = reader.Peek ()
            if isEndOfStats input then return [ statement ]
            else
                let! rest = parseStatements isEndOfStats reader
                return statement :: rest
    }

    let parseStatement (reader: Reader) = result {
        match! reader.Next () with
        | Keyword Keyword.Var ->
            match! reader.Next () with
            | Identifier id ->
                do! Help.ConsumeLexeme (Colon, reader)
                let! typeId = TypeIds.parseTypeId reader

                match! reader.Next () with
                | Equal ->
                    let! expr = Expressions.parseExpression reader
                    return Statement.VarDecl (id, typeId, Some expr)
                | input ->
                    reader.Return (Ok input)
                    return Statement.VarDecl (id, typeId, None)
            | input -> return! Help.UnexpectedLexeme input

        | Identifier id  as input ->
            reader.Return (Ok input)
            let! id = parseDotString reader

            match! reader.Next () with
            | Equal ->
                let! expr = Expressions.parseExpression reader
                return Statement.VarAssignment (id, expr)
            | LSBracket ->
                let! expr = Expressions.parseExpression reader
                return Statement.ArrayAssignment (id, expr)
            | input ->
                reader.Return (Ok input)
                let! typeArgs = TypeIds.parseTypeArguments reader
                match! reader.Next () with
                | LParen ->
                    reader.Return (Ok LParen)
                    let! args = Expressions.parseFunctionArgs reader
                    return Statement.FuncCall (id, typeArgs, args)
                | input -> return! Help.UnexpectedLexeme input

        | Keyword Keyword.If -> return! parseIfStatement reader
        | Keyword Keyword.While -> return! parseWhileStatement reader
        | Keyword Keyword.Defer -> return! parseDeferStatement reader
        | Keyword Keyword.Return -> return! parseReturnStatement reader
        | input -> return! Help.UnexpectedLexeme input
    }

    let parseIfStatement (reader: Reader) = result {
        let! expr = Expressions.parseExpression reader
        do! Help.ConsumeLexeme (Keyword Keyword.Then, reader)
        let isEndOfStat = (fun l -> l = Keyword Keyword.Fi ||
                                    l = Keyword Keyword.ElseIf ||
                                    l = Keyword Keyword.Else)

        let! statements = parseStatements isEndOfStat reader

        let rec elseIfs () = result {
            match! reader.Next () with
            | Keyword Keyword.ElseIf ->
                let! expr = Expressions.parseExpression reader
                do! Help.ConsumeLexeme (Keyword Keyword.Then, reader)
                let! statements = parseStatements isEndOfStat reader
                let! rest = elseIfs ()
                return { IfCond.Condition = expr; Body = statements } :: rest
            | input ->
                reader.Return (Ok input)
                return []
        }

        let! elseIfs = elseIfs ()
        let! elsePart = result {
            match! reader.Next () with
            | Keyword Keyword.Else ->
                return! parseStatements (fun l -> l = Keyword Keyword.Fi) reader
            | input ->
                reader.Return (Ok input)
                return []
        }
        do! Help.ConsumeLexeme (Keyword Keyword.Fi, reader)
        return Statement.If ({ Condition = expr; Body = statements }, elseIfs, elsePart)
    }

    let parseWhileStatement (reader: Reader) = result {
        let! expr = Expressions.parseExpression reader
        do! Help.ConsumeLexeme (Keyword Keyword.Do, reader)
        let! statements = parseStatements (fun l -> l = Keyword Keyword.EndWhile) reader
        return Statement.While (expr, statements)
    }

    let parseDeferStatement (reader: Reader) = result {
        let! statements = parseStatements (fun l -> l = Keyword Keyword.EndDefer) reader
        do! Help.ConsumeLexeme (Keyword Keyword.EndDefer, reader)
        return Statement.Defer statements
    }

    let parseReturnStatement (reader: Reader) = result {

        let! input = reader.Peek ()
        match input with
        | Keyword Keyword.EndFunc
        | Keyword Keyword.EndFor
        | Keyword Keyword.EndWhile
        | Keyword Keyword.EndDefer
        | Keyword Keyword.Fi
        | Keyword Keyword.Else
        | Keyword Keyword.ElseIf -> return Statement.Return

        | _ ->
            let! expr = Expressions.parseExpression reader
            return Statement.ReturnExpr expr
    }



let rec parseOpenDirectives (reader: Reader) =
    let rec parsePath () = result {
        match! reader.Next () with
        | Identifier path ->
            match! reader.Next () with
            | Operator "/" ->
                let! path2 = parsePath ()
                return sprintf $"%s{path}/%s{path2}"
            | input ->
                reader.Return (Ok input)
                return path
        | input -> return! Help.UnexpectedLexeme input
    }

    let parseAlias () = result {
        match! reader.Next () with
        | Keyword Keyword.As ->
            match! reader.Next () with
            | Identifier id -> return Some id
            | input -> return! Help.UnexpectedLexeme input
        | input ->
            reader.Return (Ok input)
            return None
    }

    result {
        do! skipNewlines reader
        match! reader.Next () with
        | Keyword Keyword.Open ->
            let! path = parsePath ()
            let! alias = parseAlias ()
            do! Help.ConsumeLexeme (Newline, reader)
            let! rest = parseOpenDirectives reader
            return { OpenDirective.Path = path; Alias = alias } :: rest
        | input ->
            reader.Return (Ok input)
            return []
    }

let parseVariableDeclaration (reader: Reader) = result {
    do! Help.ConsumeLexeme (Keyword Keyword.Var, reader)

    match! reader.Next () with
    | Identifier id  ->
        do! Help.ConsumeLexeme (Colon, reader)
        let! typeId = TypeIds.parseTypeId reader
        let! initExpr = result {
            match! reader.Next () with
            | Equal ->
                let! initExpr = Expressions.parseExpression reader
                return Some initExpr
            | input ->
                reader.Return (Ok input)
                return None
        }
        do! Help.ConsumeLexeme (Newline, reader)

        return { Variable.Name = id
                 TypeId = typeId
                 Modifiers = []
                 InitExpr = initExpr }

    | input -> return! Help.UnexpectedLexeme input
}

let parseFunctionDeclaration (reader: Reader) =
    let rec parseFuncArgs () = result {
        match! reader.Next () with
        | Identifier id ->
            do! Help.ConsumeLexeme (Colon, reader)
            let! typeId = TypeIds.parseTypeId reader
            match! reader.Next () with
            | Comma ->
                let! rest = parseFuncArgs ()
                return (id, typeId) :: rest
            | input ->
                reader.Return (Ok input)
                return [ (id, typeId) ]

        | input -> return! Help.UnexpectedLexeme input
    }

    result {
        do! Help.ConsumeLexeme (Keyword Keyword.Func, reader)

        match! reader.Next () with
        | Identifier id ->
            let! typeArgs = TypeIds.parseTypeArgumentNames reader

            do! Help.ConsumeLexeme (LParen, reader)
            let! args = parseFuncArgs ()
            do! Help.ConsumeLexeme (RParen, reader)

            let! returnType = result {
                match! reader.Next () with
                | Colon ->
                    return! TypeIds.parseTypeId reader
                | input ->
                    reader.Return (Ok input)
                    return TypeId.Void
            }
            do! Help.ConsumeLexeme (Newline, reader)
            let! statements = Statements.parseStatements (fun l -> l = Keyword Keyword.EndFunc) reader
            do! Help.ConsumeLexeme(Keyword Keyword.EndFunc, reader)

            return { Function.Name = id
                     TypeArgs = typeArgs
                     Args = args
                     ReturnType = returnType
                     Modifiers = []
                     Body = statements }

        | input -> return! Help.UnexpectedLexeme input
    }

let parseStructUnionEnum (reader: Reader) =
    let rec parseFieldsDecls () = result {
        match! reader.Next () with
        | Identifier id  ->
            do! Help.ConsumeLexeme (Colon, reader)
            let! typeId = TypeIds.parseTypeId reader
            do! Help.ConsumeLexeme (Lexeme.Newline, reader)
            do! skipNewlines reader
            let! rest = parseFieldsDecls ()
            return (id, typeId) :: rest

        | input ->
            reader.Return (Ok input)
            return []
    }

    let rec parseFuncsDecls () = result {
        match! reader.Peek () with
        | Keyword Keyword.Func ->
            let! func = parseFunctionDeclaration reader
            do! skipNewlines reader
            let! rest = parseFuncsDecls ()
            return func :: rest
        | _ -> return []
    }

    result {
        let! typeType = result {
            match! reader.Next () with
            | Keyword Keyword.Struct -> return TypeType.Struct
            | Keyword Keyword.Union -> return TypeType.Union
            | Keyword Keyword.Enum -> return TypeType.Enum
            | input -> return! Help.UnexpectedLexeme input
        }

        match! reader.Next () with
        | Identifier id ->
            let! typeArgs = TypeIds.parseTypeArgumentNames reader
            do! Help.ConsumeLexeme (Newline, reader)
            do! skipNewlines reader
            let! fields = parseFieldsDecls ()
            do! skipNewlines reader
            let! funcs = parseFuncsDecls ()

            do! result {
                let! input = reader.Next ()
                match (input, typeType) with
                | Keyword Keyword.EndStruct, TypeType.Struct -> return ()
                | Keyword Keyword.EndUnion, TypeType.Union -> return ()
                | Keyword Keyword.EndEnum, TypeType.Enum -> return ()
                | input , _ -> return! Help.UnexpectedLexeme input
            }

            return { TypeDecl.Name = id
                     TypeType = typeType
                     TypeArgs = typeArgs
                     Fields = fields
                     Functions = funcs }
        | input -> return! Help.UnexpectedLexeme input
    }

let rec parseDeclarations (reader: Reader) : Result<Declaration list, string> = result {
    do! skipNewlines reader
    match! reader.Peek () with
    | Keyword Keyword.Var ->
        let! variable = parseVariableDeclaration reader
        let variable = Declaration.Variable variable
        let! next = parseDeclarations reader
        return variable :: next

    | Keyword Keyword.Func ->
        let! func = parseFunctionDeclaration reader
        let func = Declaration.Function func
        let! next = parseDeclarations reader
        return func :: next

    | Keyword Keyword.Struct
    | Keyword Keyword.Union
    | Keyword Keyword.Enum  ->
        let! str = parseStructUnionEnum reader
        let str = Declaration.Type str
        let! next = parseDeclarations reader
        return str :: next

    | EndOfFile -> return []

    | input -> return! Help.UnexpectedLexeme input
}

let parseSource (reader: Reader) filename = result {
    let! openDirectives = parseOpenDirectives reader
    let! declarations = parseDeclarations reader
    return { Source.OpenDirectives = openDirectives
             Declarations = declarations
             Filename = filename }
}

let parseSourceFromStr (str: string) = result {
    use reader = Lexer.charReaderFromStr str
    let lexemes = Lexer.tokenize reader
    use lexemeReader = new SeqReader<Result<Lexeme, string>> (lexemes)
    let! source = parseSource lexemeReader ""
    return source
}

let parseSourceFromTextReader textReader filename = result {
    use reader = Lexer.charReaderFromTextReader textReader
    let lexemes = Lexer.tokenize reader
    use lexemeReader = new SeqReader<Result<Lexeme, string>> (lexemes)
    return! parseSource lexemeReader filename
}
