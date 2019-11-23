module rec Kedr.AST.ASTParser

open Kedr.Tokenization
open Kedr.ParserGenerator

let private ident = Symbol.create()
let private quot_str = Symbol.create()
let private num = Symbol.create()

let private VAL_ATOM = Symbol.create()
let private PREFIX_APP = Symbol.create()

let private S = Symbol.create()

let private productions = [
    S => [ PREFIX_APP ]
    PREFIX_APP => [ VAL_ATOM ]
    PREFIX_APP => [ PREFIX_APP; VAL_ATOM ]
    VAL_ATOM => [ quot_str ]
    VAL_ATOM => [ num ]
    VAL_ATOM => [ ident ]
    ]

let parse (tokens : Token seq) : Result<ValueAtom, unit> =
    Error ()