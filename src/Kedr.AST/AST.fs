namespace Kedr.AST

(*
INFIX_APP -> PREFIX_APP | PREFIX_APP OP INFIX_APP
PREFIX_APP -> VAL_ATOM | VAL_ATOM PREFIX_APP
VAL_ATOM -> STR_LIT | NUM_LIT | IDENT
*)

type TODO = private | TODO

type Identifier = string

type ValueAtom =
    | StringLiteral of string
    | IdentifierRef of Identifier

type PrefixApplication =
    | Function of ValueAtom
    | ArgumentFor of PrefixApplication * ValueAtom

type InfixApplication =
    | Start of PrefixApplication
    | NextOp of operator : TODO * left : InfixApplication * right : PrefixApplication

type AST = InfixApplication