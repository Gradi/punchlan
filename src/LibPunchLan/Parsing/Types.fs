namespace LibPunchLan.Parsing

type Source =
    { OpenDirective : OpenDirective list
      Declarations : Declaration list
      Filename : string }

and OpenDirective =
    { Path: string
      Alias: string option }

and Declaration =
    | Variable of Variable
    | Function of Function
    | Type of TypeDecl

and Variable =
    { Name: string
      TypeId: TypeId
      Modifiers: Modifier list }

and Function =
    { Name: string
      Args: (string * TypeId) list
      ReturnType: TypeId
      Modifiers: Modifier list
      Body: Statement list }

and TypeDecl =
    { Name: string
      TypeType: TypeType
      Fields: TypeId list
      Functions: Function list }

and TypeType =
    | Struct
    | Union
    | Enum

and TypeId =
    | Int8 | Uint8
    | Int16 | Uint16
    | Int32 | Uint32
    | Int64 | Uint64
    | Float | Double
    | Bool | Char
    | Function of Args: TypeId list * ReturnType: TypeId
    | Pointer of TypeId
    | Void
    | Const of TypeId

and Modifier =
    | Extern
    | Export

and Statement =
    | ToBeImplemented
