namespace LibPunchLan.Lexing

type Lexeme =
    | Keyword of Keyword
    | Identifier of string
    | Number of Number
    | LParen      // (
    | RParen      // )
    | LSBracket   // [
    | RSBracket   // ]
    | LABracket   // <
    | RABracket   // >
    | Operator of string
    | Colon
    | Comma
    | Newline
    | Equal
    | DEqual
    | NotEqual
    | LessThanOrEqual
    | GreaterThanorEqual

and Keyword =
    | Open | As
    | Func | EndFunc
    | Struct | EndStruct
    | Union | EndUnion
    | Enum | EndEnum
    | If | Else | Fi
    | Switch | Case | Break | Default | EndSwitch
    | While | EndWhile
    | For  | In | EndFor
    | Private
    | Return
    | Var
    | Extern

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

type Char =
    { Char: char
      Row: int
      Col: int }

type LexemeContainer =
    { Lexeme: Lexeme
      RowStart: int
      RowEnd: int
      ColStart: int
      ColEnd: int }
