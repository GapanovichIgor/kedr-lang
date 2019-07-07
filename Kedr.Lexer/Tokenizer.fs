module Kedr.Tokenizer

open Kedr
open System.IO
open System.Text

open ParserComposition
open ParserPrimitives
open TokenParsers
open WhiteSpace



let private parseReader (reader: StreamReader) =
    let readChar() =
        let i = reader.Read()
        if i = -1
        then None
        else Some(char i)

    let tape = Tape(readChar)
    
    let skipWhitespace = skipZeroOrMoreCond isWhiteSpace |> commitOnSuccess
    
    let parseToken =
        chooseLongest [
            number
            quotedString
        ]
        |> orElse invalidToken
        |> commitOnSuccess
        
    let parseLine =
        zeroOrMore (skipWhitespace >>. parseToken .>> skipWhitespace)

    let parse = parseLine

    match parse tape with
    | Ok ok -> ok.value
    | Error _ -> []


let parse (stream: Stream): Token list =
    let reader = new StreamReader(stream)
    parseReader reader

let parseEnc (encoding: Encoding) (stream: Stream): Token list =
    let reader = new StreamReader(stream, encoding)
    parseReader reader

let parseString (str: string): Token list =
    let encoding = Encoding.Unicode
    let bytes = encoding.GetBytes str
    let ms = new MemoryStream(bytes)
    parseEnc encoding ms
