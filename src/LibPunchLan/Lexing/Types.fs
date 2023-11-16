namespace rec LibPunchLan.Lexing

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

type Keyword =
    | Open | As
    | Func | EndFunc
    | Struct | EndStruct
    | Union | EndUnion
    | If | Then | ElseIf | Else | Fi
    | While | Do | EndWhile
    | For | In | EndFor
    | Return
    | Var
    | Extern | Export
    | Defer | EndDefer

    | Int8 | Uint8
    | Int16 | Uint16
    | Int32 | Uint32
    | Int64 | Uint64
    | Double
    | Float
    | Char
    | Bool
    | Void
    | Pointer
    | Const

    | True | False

    | And | Or | Not
    | Xor

    | Sizeof
    | Addrof
    | Deref
    | Cast

type Number =
    | Integer of DecInt array
    | HexInteger of HexInt array
    | BinaryInteger of BitInt array
    | Double of double
    | Negative of Number

type DecInt =
    | Zero | One | Two | Three | Four
    | Five | Six | Seven | Eight | Nine

type HexInt =
    | DecInt of DecInt
    | A | B | C | D | E | F

type BitInt = Zero | One

type LexemeContainer =
    { Lexeme: Lexeme
      Row: int
      Col: int }
