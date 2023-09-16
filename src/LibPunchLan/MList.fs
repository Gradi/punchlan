module LibPunchLan.MList

let rec duplicates lst =
    match lst with
    | [] -> []
    | x :: xs ->
        match List.contains x xs with
        | true -> x :: duplicates xs
        | false -> duplicates xs
