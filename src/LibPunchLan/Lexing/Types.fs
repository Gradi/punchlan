namespace LibPunchLan.Lexing

type Lexeme =
    | Keyword of Keyword
    | Identifier of string
    | Number of Number
    | String of string
    | Char of char
    | LParen      // (
    | RParen      // )
    | LSBracket   // [
    | RSBracket   // ]
    | LABracket   // <
    | RABracket   // >
    | LCBracket   // {
    | RCBracket   // }
    | Operator of string
    | Colon
    | Comma
    | Dot
    | DoubleDot
    | Newline
    | Equal
    | DEqual
    | NotEqual
    | LessThanOrEqual
    | GreaterThanOrEqual
    | EndOfFile

and Keyword =
    | Open | As
    | Func | EndFunc
    | Struct | EndStruct
    | Union | EndUnion
    | Enum | EndEnum
    | If | Then | ElseIf | Else | Fi
    | While | Do | EndWhile
    | For | In | EndFor
    | Private
    | Return
    | Var
    | Extern
    | Defer | EndDefer

    | Int8 | Uint8
    | Int16 | Uint16
    | Int32 | Uint32
    | Double
    | Float
    | Char
    | Bool
    | Sizet
    | Void
    | Pointer
    | Const

    | True | False

    | And | Or | Not
    | Xor

and Number =
    | Integer of DecInt array
    | HexInteger of HexInt array
    | BinaryInteger of BitInt array
    | Double of double
    | Negative of Number

and DecInt =
    | Zero | One | Two | Three | Four
    | Five | Six | Seven | Eight | Nine

and HexInt =
    | DecInt of DecInt
    | A | B | C | D | E | F

and BitInt = Zero | One
