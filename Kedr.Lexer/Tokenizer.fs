module Kedr.Tokenizer

open Kedr
open System.IO
open System.Text

open System.Collections.Generic
open ParserComposition
open ParserPrimitives
open TokenParsers

let private skipWhitespace =
    let whitespace = [ ' '; '\t' ] |> HashSet
    skipWhile whitespace.Contains

let private parseReader (reader: StreamReader) =
    let readChar() =
        let i = reader.Read()
        if i = -1
        then None
        else Some(char i)

    let tape = Tape(readChar)

    let parser =
        zeroOrMore (
            (skipWhitespace |> commitOnSuccess)
            >>.
            (chooseLongest [
                number
                quotedString
            ] |> commitOnSuccess)
            .>>
            (skipWhitespace |> commitOnSuccess)
        )

    match parser tape with
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