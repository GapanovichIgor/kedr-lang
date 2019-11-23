module rec Kedr.AST.ASTParser

open Kedr.Tokenization
open Kedr.ParserGenerator

let private ident = Symbol.create "ident"
let private quot_str = Symbol.create "quot_str"
let private num = Symbol.create "num"

let private VAL_ATOM = Symbol.create "VAL_ATOM"
let private PREFIX_APP = Symbol.create "PREFIX_APP"

let private S = Symbol.create "S"

let private productions = [
    S => [ PREFIX_APP ]
    PREFIX_APP => [ VAL_ATOM ]
    PREFIX_APP => [ PREFIX_APP; VAL_ATOM ]
    VAL_ATOM => [ quot_str ]
    VAL_ATOM => [ num ]
    VAL_ATOM => [ ident ]
    VAL_ATOM => [ PREFIX_APP ]
    ]

let private testProductions =
    let S = Symbol.create "S"
    let E = Symbol.create "E"
    let T = Symbol.create "T"
    let ``+`` = Symbol.create "+"
    let ``;`` = Symbol.create ";"
    let int = Symbol.create "int"
    let ``(`` = Symbol.create "("
    let ``)`` = Symbol.create ")"

    [
        S => [ E ]
        E => [ T; ``;`` ]
        E => [ T; ``+``; E ]
        T => [ int ]
        T => [ ``(``; E; ``)`` ]
    ]
    |> Set.ofList

let parse (tokens : Token seq) : Result<ValueAtom, unit> =
    let parser = LALRGenerator.generate testProductions
    Error ()