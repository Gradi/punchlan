namespace LibPunchLan.TypeChecking

open LibPunchLan.Parsing

type Program =
    { Sources: Source list }

type Diagnostic =
    { Source: Source
      Function: Function option
      Message: string }

type Diagnostics = Diagnostic list

type SourceContext =
    { CurrentSource: Source
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeId>> }

        member this.WithFunction (func: Function) =
            { SourceFunctionContext.CurrentSource = this.CurrentSource
              CurrentFunction = func
              Program = this.Program
              NameTypeEnv = this.NameTypeEnv
              DeclaredVariables = lazy Set.empty }

and SourceFunctionContext =
    { CurrentSource: Source
      CurrentFunction: Function
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeId>>
      DeclaredVariables: Lazy<Set<string>> }

        member this.WithSource () =
            { SourceContext.CurrentSource = this.CurrentSource
              Program = this.Program
              NameTypeEnv = this.NameTypeEnv }

        member this.WithTypeEnv name typ =
            { this with NameTypeEnv = lazy (Map.add name typ this.NameTypeEnv.Value) }

        member this.WithVariable name =
            { this with DeclaredVariables = lazy (Set.add name this.DeclaredVariables.Value)}
