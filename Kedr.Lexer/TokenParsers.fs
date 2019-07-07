module internal Kedr.TokenParsers
open System
open ParserPrimitives
open ParserComposition
open WhiteSpace

let private parseResultMapConst x = ParseResult.map (fun _ -> x)

let private charToken c t = skipOne c >> parseResultMapConst t 

let quotedString: Parser<char, Token> =
    (skipOne '"' >>. zeroOrMoreAnyWithTerminator '"')
    >> ParseResult.map (String >> QuotedString)

let number: Parser<char, Token> =
    let integerPart = oneOrMoreCond Char.IsDigit
    let fractionalPart = optional (skipOne '.' >>. oneOrMoreCond Char.IsDigit)

    let makeToken (integerPart: char array, fractionalPart: char array option) =
        let integerPart = integerPart |> String |> Int64.Parse
        let fractionalPart = fractionalPart |> Option.map (String >> Int64.Parse)
        Number(integerPart, fractionalPart)

    integerPart .>>. fractionalPart
    >> ParseResult.map makeToken

let plus: Parser<char, Token> = charToken '+' Plus
    
let minus: Parser<char, Token> = charToken '-' Minus

let invalidToken: Parser<char, Token> =
    oneOrMoreCond (not << isWhiteSpace)
    >> ParseResult.map (String >> InvalidToken)
