module Kedr.Tokenizer

open System
open Kedr
open System.IO
open System.Text

open System.Collections.Generic
open Kedr
open ParserComposition
open ParserPrimitives
open TokenParsers

let parse (encoding : Encoding) (stream : Stream) : Token list =
    let reader = new StreamReader(stream, encoding)
    
    let readChar () =
        let i = reader.Read()
        if i = -1
        then None
        else Some (char i)
        
    let tape = Tape(readChar)
    
    let skipWhitespace =
        let whitespace = [ ' '; '\t' ] |> HashSet
        skipWhile whitespace.Contains
    
    let parser =
        zeroOrMore(
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
    
    
let parseString (str : string) : Token list =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding ms