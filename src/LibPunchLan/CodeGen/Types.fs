namespace LibPunchLan.CodeGen

open LibPunchLan.TypeChecking

type ICodegenerator =

    abstract Write : unit -> unit
