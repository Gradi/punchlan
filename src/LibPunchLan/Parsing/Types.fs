namespace rec LibPunchLan.Parsing

open LibPunchLan.Lexing

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

type Variable =
    { Name: string
      TypeId: TypeId
      Modifiers: Modifier list
      InitExpr: Expression option }

type Function =
    { Name: string
      TypeArgs: string list
      Args: (string * TypeId) list
      ReturnType: TypeId
      Modifiers: Modifier list
      Body: Statement list }

type TypeDecl =
    { Name: string
      TypeType: TypeType
      TypeArgs: string list
      Fields: (string * TypeId) list
      Functions: Function list }

type TypeType =
    | Struct
    | Union
    | Enum

type TypeId =
    | Int8 | Uint8
    | Int16 | Uint16
    | Int32 | Uint32
    | Int64 | Uint64
    | Float | Double
    | Bool | Char | Sizet | Void
    | Pointer of TypeId
    | Const of TypeId
    | Named of Name: DotString * TypeArgs: TypeId list

type Modifier =
    | Extern
    | Export

type Statement =
    | VarDecl of string * TypeId * Expression option
    | VarAssignment of DotString * Expression
    | ArrayAssignment of DotString * Expression
    | FuncCall of DotString * TypeId list * Expression list
    | If of Main: IfCond * ElseIf: IfCond list * Else: Statement list
    | For of Variable: string * Expression * Expression * Expression option * Statement list
    | While of Expression * Statement list
    | Defer of Statement list
    | Return
    | ReturnExpr of Expression

and IfCond =
    { Condition: Expression
      Body: Statement list }

type Expression =
    | Constant of Value
    | Variable of string
    | FuncCall of string * TypeId list * Expression list
    | MemberCall of Expression * string * TypeId list * Expression list
    | MemberAccess of Expression * string
    | BinaryExpression of BinaryExpression
    | ArrayAccess of Expression * Expression
    | StructCreation of DotString * TypeId list * (string * Expression) list
    | AddressOf of string
    | Bininversion of Expression

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

type DotString = string list
