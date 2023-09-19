module LibPunchLan.MList

let rec duplicates lst =
    match lst with
    | [] -> []
    | x :: xs ->
        match List.contains x xs with
        | true -> x :: duplicates xs
        | false -> duplicates xs

let rec tryLookup key alist =
    match alist with
    | [] -> None
    | (x, elem) :: xs ->
        match key = x with
        | true -> Some elem
        | false -> tryLookup key xs
