namespace LibPunchLan.TypeChecking

open LibPunchLan.Parsing

[<NoComparison;ReferenceEquality>]
type Program =
    { Sources: Source list }

[<NoComparison;ReferenceEquality>]
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

type VarRef =
    { Variable: Variable
      Source: Source }

    override this.ToString () = sprintf $"%s{this.Source.Filename}:%s{this.Variable.Name}"

type FuncRef =
    { Function: Function
      Source: Source }

    override this.ToString () = sprintf $"%s{this.Source.Filename}:%s{this.Function.Name}"

type TypeRef =
    { TypeId: TypeId
      Source: Source }

    override this.ToString () = sprintf $"%s{this.Source.Filename}:%O{this.TypeId}"

type TypeDeclRef =
    { TypeDecl: TypeDecl
      Source: Source }

    override this.ToString () = sprintf $"%s{this.Source.Filename}:%s{this.TypeDecl.Name}"

[<NoComparison;ReferenceEquality>]
type SourceContext =
    { CurrentSource: Source
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeRef>> }

    member this.WithFunction (func: Function) =
        { SourceFunctionContext.CurrentSource = this.CurrentSource
          CurrentFunction = func
          Program = this.Program
          NameTypeEnv = this.NameTypeEnv }

and SourceFunctionContext =
    { CurrentSource: Source
      CurrentFunction: Function
      Program: Program
      NameTypeEnv: Lazy<Map<string, TypeRef>> }

    member this.WithSource () =
        { SourceContext.CurrentSource = this.CurrentSource
          Program = this.Program
          NameTypeEnv = this.NameTypeEnv }
