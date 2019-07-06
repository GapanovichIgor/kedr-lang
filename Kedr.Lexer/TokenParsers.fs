module internal Kedr.TokenParsers
open System
open ParserPrimitives
open ParserComposition

let quotedString tape : ParseResult<Token> =
    tape
    |> (skip '"' >>. anyWithTerminator '"')
    |> ParseResult.map (String >> QuotedString)


let number tape : ParseResult<Token> =
    let parse =
        takeWhile Char.IsDigit
        |> combine (optional (skip '.' >>. takeWhile Char.IsDigit))
            (fun integerPart fractionalPart ->
                let integerPart = integerPart |> String |> Int64.Parse
                let fractionalPart = fractionalPart |> Option.map (String >> Int64.Parse)
                Number (integerPart, fractionalPart))
            
    parse tape