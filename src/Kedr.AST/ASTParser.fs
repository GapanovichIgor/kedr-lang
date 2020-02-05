module rec Kedr.AST.ASTParser

open Kedr.Tokenization
open Kedr.ParserGenerator

//let private ident = Symbol "ident"
//let private quot_str = Symbol "quot_str"
//let private num = Symbol "num"
//
//let private VAL_ATOM = Symbol "VAL_ATOM"
//let private PREFIX_APP = Symbol "PREFIX_APP"
//
//let private S = Symbol "S"
//
//let private productions = [
//    S => [ PREFIX_APP ]
//    PREFIX_APP => [ VAL_ATOM ]
//    PREFIX_APP => [ PREFIX_APP; VAL_ATOM ]
//    VAL_ATOM => [ quot_str ]
//    VAL_ATOM => [ num ]
//    VAL_ATOM => [ ident ]
//    VAL_ATOM => [ PREFIX_APP ]
//    ]

let private testGrammar =
    [
        "S" => [ "E" ]
        "E" => [ "L"; "="; "R" ]
        "E" => [ "R" ]
        "L" => [ "id" ]
        "L" => [ "*"; "R" ]
        "R" => [ "L" ]
    ]
    |> Set.ofList
    |> Grammar.fromProductions

let parse (tokens : Token seq) : Result<ValueAtom, unit> =
    let parser = Automaton.create testGrammar
    Error ()