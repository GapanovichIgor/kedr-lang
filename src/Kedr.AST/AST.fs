namespace Kedr.AST

type Identity = string

type TypeId = Identity // for now

type Expr =
    | IdRef of Identity
    | NumLit of integral : uint32 * fractional : uint32 option
    | StrLit of string
    | Application of Expr * Expr

type BindingParameter =
    { name : Identity
      typeAnnotation : TypeId option }

type Binding =
    { name : Identity
      parameters : BindingParameter list
      typeAnnotation : TypeId option
      body : Expr }

type ModuleMember =
    | Value of Binding

type Module =
    { name : Identity
      members : ModuleMember list }

type Program =
    | PExpr of Expr
    | PBinding of Binding
    | PModule of Module