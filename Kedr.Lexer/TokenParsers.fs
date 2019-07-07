module internal Kedr.TokenParsers
open System
open ParserPrimitives
open ParserComposition
open WhiteSpace

let private parseResultMapConst x = ParseResult.map (fun _ -> x)

let private constToken (str: string) t =
    assert (str.Length > 0)

    let parser =
        if str.Length = 1 then
            skipOne str.[0]
        else
            str
            |> Seq.map skipOne
            |> Seq.reduce (>>.)

    parser >> parseResultMapConst t

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

let plus: Parser<char, Token> = constToken "+" Plus

let minus: Parser<char, Token> = constToken "-" Minus

let asterisk: Parser<char, Token> = constToken "*" Asterisk

let slash: Parser<char, Token> = constToken "/" Slash

let equals: Parser<char, Token> = constToken "=" Equals

let notEquals: Parser<char, Token> = constToken "/=" NotEquals

let parenOpen: Parser<char, Token> = constToken "(" ParenOpen

let parenClose: Parser<char, Token> = constToken ")" ParenClose

let invalidToken: Parser<char, Token> =
    oneOrMoreCond (not << isWhiteSpace)
    >> ParseResult.map (String >> InvalidToken)
