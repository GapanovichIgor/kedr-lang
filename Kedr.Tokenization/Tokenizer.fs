module Kedr.Tokenization.Tokenizer

open ParserComposition
open ParserPrimitives
open System.IO
open System.Text
open TokenParsers
open WhiteSpace

let private parseReader (reader : StreamReader) =
    let readChar() =
        let i = reader.Read()
        if i = -1
        then None
        else Some(char i)

    let tape = Tape(readChar)

    let maybeSkipWhitespace = skipZeroOrMoreCond isWhiteSpace |> commitOnSuccess

    let skipNewLine =
        chooseFirstLongest [
            skipOne '\n'
            skipOne '\r'
            skipOne '\r' >>. skipOne '\n'
        ]
        |> commitOnSuccess

    let parseToken =
        chooseFirstLongest [
            let'
            type'
            identifier

            plus
            minus
            asterisk
            slash
            equals
            notEquals
            parenOpen
            parenClose
            squareBracketOpen
            squareBracketClose
            hardBreak

            number

            quotedString
        ]
        |> orElse invalidToken
        |> commitOnSuccess

    let parseLineWithTokens =
        IndentationParser.parse .>>. oneOrMore (parseToken .>> maybeSkipWhitespace)
        >> ParseResult.mapValue (fun (l, r) -> l @ r)

    let parseEmptyLine =
        maybeSkipWhitespace
        >> ParseResult.mapValue (fun _ -> [])

    let parseLine =
        parseLineWithTokens
        |> orElse parseEmptyLine

    let terminateBlocks result =
        match result with
        | Ok s ->
            let (tokens, state) = IndentationParser.terminateWhenEndReached s.state
            { value = s.value @ [ tokens ]
              state = state
              length = s.length }
            |> Ok
        | Error e -> Error e

    let parse =
        zeroOrMoreDelimited skipNewLine parseLine
        >> terminateBlocks
        >> ParseResult.mapValue List.concat

    match parse (tape, TokenizerState.initial) with
    | Ok ok ->
        { tokens = ok.value
          isIndentWhitespaceMixed = ok.state.indentationStyle = Some IndentationStyle.Mixed }
    | Error _ ->
        assert false
        { tokens = []; isIndentWhitespaceMixed = false }


let parse (stream : Stream) : TokenizerResult =
    let reader = new StreamReader(stream)
    parseReader reader

let parseEnc (encoding : Encoding) (stream : Stream) : TokenizerResult =
    let reader = new StreamReader(stream, encoding)
    parseReader reader

let parseString (str : string) : TokenizerResult =
    let encoding = Encoding.Unicode
    let bytes = encoding.GetBytes str
    let ms = new MemoryStream(bytes)
    parseEnc encoding ms
