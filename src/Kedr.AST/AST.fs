namespace Kedr.AST

type Identity = string

type Expr =
    | InParens of Expr
    | IdRef of Identity
    | NumLit of integral : uint32 * fractional : uint32 option
    | StrLit of string
    | Application of Expr * Expr
