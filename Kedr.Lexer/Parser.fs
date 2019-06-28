module Kedr.Parser

open System.IO
open System.Text
open FParsec

type Token =
    | Number of string
    | StringLiteral of contents:string

type private ParserState = unit

let private pNumberLiteral =
    regex "-?\d+(\.\d+)?" // IT NORMALIZES NEWLINES
    |>> Number

let private pStringLiteral =
    regex "\"[^\"]*\""
    |>> (fun str ->
        let contents = str.Substring(1, str.Length - 2)
        StringLiteral contents )

let private pToken =
    choice [
        pNumberLiteral
        pStringLiteral
    ]

let private kedrParser =
    (
        spaces >>. (sepEndBy1 pToken spaces1)
    )
    .>>
    eof

let parse (encoding : Encoding) (streamName : string, stream : Stream) : Result<Token list, string> =
    let state = ()
    let parseResult = runParserOnStream kedrParser state streamName stream encoding

    match parseResult with
    | Success (tokens, _, _) ->
        Result.Ok tokens
    | Failure (errorStr, error, _) ->
        Result.Error errorStr

let parseString (name : string, str : string) : Result<Token list, string> =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding (name, ms)