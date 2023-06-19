module LibPunchLan.Lexing.NumberMod

open LibPunchLan.Lexing

let private charToHex char =
    match char with
    | '0' -> Some (HexInt.DecInt DecInt.Zero)
    | '1' -> Some (HexInt.DecInt DecInt.One)
    | '2' -> Some (HexInt.DecInt DecInt.Two)
    | '3' -> Some (HexInt.DecInt DecInt.Three)
    | '4' -> Some (HexInt.DecInt DecInt.Four)
    | '5' -> Some (HexInt.DecInt DecInt.Five)
    | '6' -> Some (HexInt.DecInt DecInt.Six)
    | '7' -> Some (HexInt.DecInt DecInt.Seven)
    | '8' -> Some (HexInt.DecInt DecInt.Eight)
    | '9' -> Some (HexInt.DecInt DecInt.Nine)
    | 'a' | 'A'  -> Some HexInt.A
    | 'b' | 'B'  -> Some HexInt.B
    | 'c' | 'C'  -> Some HexInt.C
    | 'd' | 'D'  -> Some HexInt.D
    | 'e' | 'E'  -> Some HexInt.E
    | 'f' | 'F'  -> Some HexInt.F
    | _ -> None

let private charToBit char =
    match char with
    | '0' -> Some BitInt.Zero
    | '1' -> Some BitInt.One
    | _ -> None

let private charToDec char =
    match char with
    | '0' -> Some DecInt.Zero
    | '1' -> Some DecInt.One
    | '2' -> Some DecInt.Two
    | '3' -> Some DecInt.Three
    | '4' -> Some DecInt.Four
    | '5' -> Some DecInt.Five
    | '6' -> Some DecInt.Six
    | '7' -> Some DecInt.Seven
    | '8' -> Some DecInt.Eight
    | '9' -> Some DecInt.Nine
    | _ -> None

let tryParseNumber (string: string) =
    if System.String.IsNullOrWhiteSpace string then None

    else if string.StartsWith "0x" || string.StartsWith "0X" then
        let hexNumbers =
            string.Substring 2
            |> Seq.map charToHex
            |> Array.ofSeq

        if Array.exists Option.isNone hexNumbers then None
        else
            Some <| (Number.HexInteger (hexNumbers |> Array.choose id))

    else if string.StartsWith "0b" || string.StartsWith "0B" then
        let binNumbers =
            string.Substring 2
            |> Seq.map charToBit
            |> Array.ofSeq

        if Array.exists Option.isNone binNumbers then None
        else
            Some <| (Number.BinaryInteger (binNumbers |> Array.choose id))

    else
        let decNumber =
            let decNumbers =
                string
                |> Seq.map charToDec
                |> Array.ofSeq

            if Array.exists Option.isNone decNumbers then None
            else
                Some <| (Number.Integer (decNumbers |> Array.choose id))

        match decNumber with
        | Some _ -> decNumber
        | None ->
            match System.Double.TryParse (string, System.Globalization.CultureInfo.InvariantCulture) with
            | true, number -> Some <| Number.Double number
            | false, _ -> None
