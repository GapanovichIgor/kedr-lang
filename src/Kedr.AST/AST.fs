namespace Kedr.AST

type Identifier = string

type ValueExpr =
    | WithBinding of identifier : Identifier * boundExpr : ValueExpr * inExpr : ValueExpr
    | StringLiteral of content : string
    | IdentifierRef of identifier : Identifier
    | Application of fn : ValueExpr * argument : ValueExpr
    | BinaryApplication of fnIdentifier : Identifier * left : ValueExpr * right : ValueExpr

//type ValueBinding = ValueBinding of identifier : Identifier * value : ValueExpr * inExpr : ValueExpr