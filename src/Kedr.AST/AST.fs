namespace rec Kedr.AST

type TODO = private | TODO

type Identifier = string

type ValueAtom =
    | StringLiteral of string
    | IdentifierRef of Identifier

//type PrefixApplication =
//    | Function of ValueAtom
//    | ArgumentFor of PrefixApplication * ValueAtom
//
//type InfixApplication =
//    | Start of PrefixApplication
//    | NextOp of operator : TODO * left : InfixApplication * right : PrefixApplication

type AST = ValueAtom