namespace rec LibPunchLan.Parsing

open LibPunchLan.Lexing

[<NoComparison;ReferenceEquality>]
type Source =
    { OpenDirectives : OpenDirective list
      Declarations : Declaration list
      Filename : string }

type OpenDirective =
    { Path: string
      Alias: string option }

type Declaration =
    | Variable of Variable
    | Function of Function
    | Type of TypeDecl

[<NoComparison;ReferenceEquality>]
type Variable =
    { Name: string
      TypeId: TypeId
      Modifier: Modifier option
      InitExpr: Expression option }

[<NoComparison;ReferenceEquality>]
type Function =
    { Name: string
      Args: (string * TypeId) list
      ReturnType: TypeId
      Modifier: Modifier option
      Body: Statement list }

[<NoComparison;ReferenceEquality>]
type TypeDecl =
    { Name: string
      TypeType: TypeType
      Fields: (string * TypeId) list }

type TypeType =
    | Struct
    | Union

type TypeId =
    | Int8 | Uint8
    | Int16 | Uint16
    | Int32 | Uint32
    | Int64 | Uint64
    | Float | Double
    | Bool | Char | Void
    | Pointer of TypeId
    | Const of TypeId
    | Named of Name: DotString

        override this.ToString () =
            match this with
            | Int8 -> "int8"
            | Uint8 -> "uint8"
            | Int16 -> "int16"
            | Uint16 -> "uint16"
            | Int32 -> "int32"
            | Uint32 -> "uint32"
            | Int64 -> "int64"
            | Uint64 -> "uint64"
            | Float -> "float"
            | Double -> "double"
            | Bool -> "bool"
            | Char -> "char"
            | Void -> "void"
            | Pointer typ -> sprintf $"pointer<%O{typ}>"
            | Const typ -> sprintf $"const %O{typ}"
            | Named name  -> name.ToString ()

type Modifier =
    | Extern
    | Export

[<NoComparison;ReferenceEquality>]
type Statement =
    | VarDecl of string * TypeId * Expression option
    | VarAssignment of Expression * Expression
    | If of Main: IfCond * ElseIf: IfCond list * Else: Statement list
    | For of Variable: string * Expression * Expression * Expression option * Statement list
    | While of Expression * Statement list
    | Defer of Statement list
    | Return
    | ReturnExpr of Expression
    | Expression of Expression

and IfCond =
    { Condition: Expression
      Body: Statement list }

[<NoComparison;ReferenceEquality>]
type Expression =
    | Constant of Value
    | Variable of string
    | FuncCall of DotString * Expression list
    | MemberAccess of Expression * string
    | BinaryExpression of BinaryExpression
    | ArrayAccess of Expression * Expression
    | StructCreation of DotString * (string * Expression) list
    | Bininversion of Expression

[<NoComparison;ReferenceEquality>]
type BinaryExpression =
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | Multiply of Expression * Expression
    | Division of Expression * Expression

    | Equal of Expression * Expression
    | NotEqual of Expression * Expression
    | Less of Expression * Expression
    | LessOrEqual of Expression * Expression
    | Greater of Expression * Expression
    | GreaterOrEqual of Expression * Expression

    | Or of Expression * Expression
    | And of Expression * Expression

    | Xor of Expression * Expression
    | RShift of Expression * Expression
    | LShift of Expression * Expression

type Value =
    | String of string
    | Number of Number
    | Boolean of bool
    | Char of char

type DotString =
    { Name: string
      Alias: string option }

        override this.ToString () =
            let alias =
                match this.Alias with
                | Some str -> sprintf $"%s{str}."
                | None -> ""
            sprintf $"%s{alias}%s{this.Name}"
