module Kedr.Parser

open System.IO
open System.Text
open FParsec

type Token =
    | Number of string

type private ParserState = unit

let private numberParser = regex "\d+(\.\d+)?" |>> Number
let private kedrParser = numberParser

let parse (encoding : Encoding) (streamName : string, stream : Stream) : Result<Token list, string> =
    let state = ()
    let parseResult = runParserOnStream kedrParser state streamName stream encoding
    
    match parseResult with
    | Success (tokens, _, _) ->
        Result.Ok (tokens |> List.singleton)
    | Failure (errorStr, error, _) ->
        Result.Error errorStr
    
let parseString (name : string, str : string) : Result<Token list, string> =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding (name, ms)