module Kedr.AST.ASTParser
#nowarn "40"

open Kedr.Parsing
open Kedr.Parsing.Primitives
open Kedr.Parsing.Composition
open Kedr.Tokenization

let private tapeOfTokenSeq (tokens : Token seq) =
    use enumerator = tokens.GetEnumerator()
    let getNext() =
        if enumerator.MoveNext() then
            Some enumerator.Current
        else
            None

    Tape(4096, getNext)

let private fromLazyFn (fn : Lazy<_>) x = fn.Value x

let private ignoreError x = x |> ParseResult.mapError ignore

let private skipOne t = skipOne t >> ignoreError

let private identifier : Parser<Token, unit, unit, Identifier> =
    tryMapOne (fun tok ->
        match tok with
        | Token.Identifier i -> Some i
        | _ -> None)
    >> ignoreError

let private skipAnyBreak : Parser<Token, unit, unit, unit> =
    chooseFirst [ skipOne Token.SoftBreak; skipOne Token.HardBreak ]
    >> ignoreError

let rec private expr : Parser<Token, unit, unit, ValueExpr> =
    lazy (
        chooseFirst [
            binding
            applyLayer
        ]
        >> ParseResult.mapError ignore
    )
    |> fromLazyFn

and private binding : Parser<Token, unit, unit, ValueExpr> =
    skipOne Token.Let >>. identifier .>> skipOne Token.Equals
    .>>. expr .>> skipAnyBreak
    .>>. expr
    >> ParseResult.mapValue (fun ((identifier, boundValue), inExpr) ->
        WithBinding(identifier, boundValue, inExpr))

and private applyLayer : Parser<Token, unit, unit, ValueExpr> =
    lazy (
        chooseFirst [
            applyLayer .>>. binaryOpLayer >> ParseResult.mapValue Application
            binaryOpLayer
        ]
        >> ignoreError
    )
    |> fromLazyFn

and private binaryOpLayer : Parser<Token, unit, unit, ValueExpr> =
    exprAtomicLayer // TODO

and private exprAtomicLayer : Parser<Token, unit, unit, ValueExpr> =
    chooseFirst [
        identifierRef
        stringLiteral
    ]
    >> ignoreError

and private identifierRef : Parser<Token, unit, unit, ValueExpr> =
    identifier >> ParseResult.mapValue (fun i -> IdentifierRef i)

and private stringLiteral : Parser<Token, unit, unit, ValueExpr> =
    tryMapOne (fun tok ->
        match tok with
        | Token.QuotedString content -> Some(StringLiteral content)
        | _ -> None)
    >> ignoreError

//and private identifierValueExprParser  : Parser<Token, unit, PrimitiveError<Token>, ValueExpr> =
//    tryMapOne (fun tok ->
//        match tok with
//        | Token.Identifier i -> Some(ValueExpr.IdentifierRef i)
//        | _ -> None)
//
//and private applicationValueExprParser : Parser<Token, unit, PrimitiveError<Token>, ValueExpr> =
//    let valueExprParser = fromLazyFn (valueExprParser)
//
//    valueExprParser .>>. valueExprParser
//    >> ParseResult.mapValue ValueExpr.Application


let parseValueExpression (tokens : Token seq) : Result<ValueExpr, unit> =
    let tape = tapeOfTokenSeq tokens

    match expr (tape, ()) with
    | Ok o -> Ok(o.value)
    | Error _ -> Error()
