module LibPunchLan.CodeGen.CodeUtils

open LibPunchLan.Parsing
open LibPunchLan.TypeChecking.TypeChecker
open LibPunchLan.TypeChecking.TypeCheckerM
open System
open System.Text

let string2label (string: string) =
    let bytes = Encoding.UTF8.GetBytes string
    let md5 = System.Security.Cryptography.MD5.HashData bytes
    let hex = Convert.ToHexString md5
    let hex = hex.ToLower ()
    sprintf $"string_%s{hex}"
