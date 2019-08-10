module Kedr.AST.ASTParser

open Kedr.Parsing
open Kedr.Parsing.Primitives
open Kedr.Parsing.Composition
open Kedr.Tokenization

let private tapeOfTokenSeq (tokens : Token seq) =
    use enumerator = tokens.GetEnumerator()
    let getNext () =
        if enumerator.MoveNext() then
            Some enumerator.Current
        else
            None
            
    Tape(4096, getNext)

let private identifierValueExprParser =
    tryMapOne (fun tok ->
        match tok with
        | Token.Identifier i -> Some (ValueExpr.IdentifierRef i)
        | _ -> None)
    
let private valueExprParser =
//    chooseFirstLongest [
        identifierValueExprParser
//    ]

let parseValueExpression (tokens : Token seq) : Result<ValueExpr, unit> =
    let tape = tapeOfTokenSeq tokens
    
    match valueExprParser (tape, ()) with
    | Ok o -> Ok (o.value)
    | Error _ -> Error ()