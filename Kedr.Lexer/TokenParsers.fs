module internal Kedr.TokenParsers
open System
open ParserPrimitives
open ParserComposition
open WhiteSpace

type private TokenParser = Parser<char, Token>

let private parseResultMapConst x = ParseResult.map (fun _ -> x)

let private constToken (str: string) t: TokenParser =
    assert (str.Length > 0)

    let parser =
        if str.Length = 1 then
            skipOne str.[0]
        else
            str
            |> Seq.map skipOne
            |> Seq.reduce (>>.)

    parser >> parseResultMapConst t

let let': TokenParser = constToken "let" Let

let type': TokenParser = constToken "type" Token.Type

let plus: TokenParser = constToken "+" Plus

let minus: TokenParser = constToken "-" Minus

let asterisk: TokenParser = constToken "*" Asterisk

let slash: TokenParser = constToken "/" Slash

let equals: TokenParser = constToken "=" Equals

let notEquals: TokenParser = constToken "/=" NotEquals

let parenOpen: TokenParser = constToken "(" ParenOpen

let parenClose: TokenParser = constToken ")" ParenClose

let identifier: TokenParser =
    let firstCharCond c =
        Char.IsLetter c ||
        c = '_'

    let followingCharCond c =
        Char.IsLetter c ||
        c = '_'
        
    let makeToken (firstChar : char, followingChars : char array) =
        let result = Array.zeroCreate<char> (followingChars.Length + 1)
        result.[0] <- firstChar
        for i = 0 to followingChars.Length - 1 do
            result.[i + 1] <- followingChars.[i]
        result
        |> String
        |> Identifier

    oneCond firstCharCond .>>. zeroOrMoreCond followingCharCond
    >> ParseResult.map makeToken

let number: TokenParser =
    let integerPart = oneOrMoreCond Char.IsDigit
    let fractionalPart = optional (skipOne '.' >>. oneOrMoreCond Char.IsDigit)

    let makeToken (integerPart: char array, fractionalPart: char array option) =
        let integerPart = integerPart |> String |> UInt32.Parse
        let fractionalPart = fractionalPart |> Option.map (String >> UInt32.Parse)
        Number(integerPart, fractionalPart)

    integerPart .>>. fractionalPart
    >> ParseResult.map makeToken

let quotedString: TokenParser =
    (skipOne '"' >>. zeroOrMoreAnyWithTerminator '"')
    >> ParseResult.map (String >> QuotedString)

let invalidToken: TokenParser =
    oneOrMoreCond (not << isWhiteSpace)
    >> ParseResult.map (String >> InvalidToken)
