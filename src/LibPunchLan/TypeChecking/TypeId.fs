module LibPunchLan.TypeChecking.TypeId

open LibPunchLan.Parsing

let integerTypes =
    [ TypeId.Int8; TypeId.Uint8; TypeId.Int16; TypeId.Uint16; TypeId.Int32; TypeId.Uint32
      TypeId.Int64; TypeId.Uint64 ]

let numericTypes = integerTypes @ [ TypeId.Float; TypeId.Double ]

let rec unwrapPointerAndConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapPointerAndConst typ
    | Pointer typ -> unwrapPointerAndConst typ
    | typ -> typ

let rec unwrapConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapConst typ
    | typ -> typ

let isConst (typ: TypeId) =
    match typ with
    | Const _ -> true
    | _ -> false

let isPointer (typ: TypeId) =
    match unwrapConst typ with
    | Pointer _ -> true
    | _ -> false

let isIntegerType (typ: TypeId) = List.contains (unwrapConst typ) integerTypes

let isNumericType (typ: TypeId) = List.contains (unwrapConst typ) numericTypes

let isFloat (typ: TypeId) =
    match unwrapConst typ with
    | Double
    | Float -> true
    | _ -> false

let isUnsigned (typ: TypeId) =
    match unwrapConst typ with
    | Uint8
    | Uint16
    | Uint32
    | Uint64 -> true
    | _ -> false

let isSigned (typ: TypeId) =
    match unwrapConst typ with
    | Int8
    | Int16
    | Int32
    | Int64 -> true
    | _ -> false

let isBool (typ: TypeId) =
    match unwrapConst typ with
    | Bool -> true
    | _ -> false

let isVoid (typ: TypeId) =
    match unwrapConst typ with
    | Void -> true
    | _ -> false

let isTypesEqual (expected: TypeRef) (actual: TypeRef) : bool =
    let left = unwrapConst expected.TypeId
    let right = unwrapConst actual.TypeId

    if isSigned left && isSigned right then true

    elif isUnsigned left && isUnsigned right then true

    elif isFloat left && isFloat right then true

    elif isPointer left && isUnsigned right then true

    else
        match unwrapConst left, unwrapConst right with
        | Named _, Named _ ->
            unwrapConst expected.TypeId = unwrapConst actual.TypeId &&
            expected.Source = actual.Source
        | _ -> left = right
