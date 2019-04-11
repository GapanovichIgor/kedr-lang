module Kedr.Parser

open System.IO
open System.Text
open FParsec

type Token =
    | Number of string

type private ParserState = unit

let private pNumber =
    regex "-?\d+(\.\d+)?"
    |>> Number

let private pExpression =
    pNumber

let private kedrParser =
    (
        spaces >>. (sepEndBy1 pExpression spaces1)
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