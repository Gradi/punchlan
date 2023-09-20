module LibPunchLan.TypeId

open LibPunchLan.Parsing

let integerTypes =
    [ TypeId.Int8; TypeId.Uint8; TypeId.Int16; TypeId.Uint16; TypeId.Int32; TypeId.Uint32
      TypeId.Int64; TypeId.Uint64 ]

let numericTypes =
    [ TypeId.Int8; TypeId.Uint8; TypeId.Int16; TypeId.Uint16; TypeId.Int32; TypeId.Uint32
      TypeId.Int64; TypeId.Uint64; TypeId.Float; TypeId.Double ]

let rec unwrapPointerAndConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapPointerAndConst typ
    | Pointer typ -> unwrapPointerAndConst typ
    | typ -> typ

let rec unwrapConst (typ: TypeId) =
    match typ with
    | Const typ -> typ
    | typ -> typ

let rec unwrapAllConst (typ: TypeId) =
    match typ with
    | Const typ -> unwrapAllConst typ
    | typ -> typ

let isConstType (typ: TypeId) =
    match typ with
    | Const _ -> true
    | _ -> false

let isPointerType (typ: TypeId) =
    match typ with
    | TypeId.Pointer _
    | Const (TypeId.Pointer _) -> true
    | _ -> false

let isTypesEqual (expected: TypeId) (actual: TypeId) =
    if numericTypes |> List.contains (unwrapConst expected) &&
          numericTypes |> List.contains (unwrapConst actual) then true
    else
        match expected with
        | Const expected ->
            match actual with
            | Const actual -> expected = actual
            | actual -> actual = expected
        | expected ->
            match actual with
            | Const _ -> false
            | actual -> expected = actual
