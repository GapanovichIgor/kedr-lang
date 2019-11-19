namespace Kedr.AST

type Identifier = string

type ValueExpr =
    | StringLiteral of content : string
    | IdentifierRef of identifier : Identifier
    | FunctionApplication of fn : ValueExpr * arg : ValueExpr

type ValueBinding = ValueBinding of identifier : Identifier * value : ValueExpr 
