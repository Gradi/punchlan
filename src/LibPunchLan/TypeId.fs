module LibPunchLan.TypeId

open LibPunchLan.Parsing

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
    match expected with
    | Const expected ->
        match actual with
        | Const actual -> expected = actual
        | actual -> actual = expected
    | expected ->
        match actual with
        | Const _ -> false
        | actual -> expected = actual
