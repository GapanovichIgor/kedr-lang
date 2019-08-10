namespace Kedr.AST

type Identifier = string

type ValueExpr =
    | StringLiteral of content : string
    | IdentifierRef of identifier : Identifier
    | FunctionApplication of fnIdentifier : Identifier * arguments : ValueExpr list

type ValueBinding = ValueBinding of identifier : Identifier * value : ValueExpr 
