namespace LibPunchLan.TypeChecking

open LibPunchLan.Parsing

type Program =
    { Sources: Source list }

type Diagnostic =
    { Source: Source
      Function: Function option
      Message: string }

        override this.ToString () =
            let func =
                match this.Function with
                | Some func -> sprintf $":func %s{func.Name}"
                | None -> ""

            sprintf $"%s{this.Source.Filename}%s{func}: %s{this.Message}"

type Diagnostics = Diagnostic list

type SourceContext =
    { CurrentSource: Source
      CurrentFunction: Function option
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeId>> }
