module internal Kedr.Tokenization.TokenParsers
open ParserComposition
open ParserPrimitives
open System
open WhiteSpace

type private TokenParser<'s> = Parser<char, 's, Token>

let private parseResultMapConst x = ParseResult.mapValue (fun _ -> x)

let private constParser (str : string) : Parser<char, 's, unit> =
    assert (str.Length > 0)

    if str.Length = 1 then
        skipOne str.[0]
    else
        str
        |> Seq.map skipOne
        |> Seq.reduce (>>.)

let private constToken (str : string) (t) : TokenParser<'s> =
    constParser str >> parseResultMapConst t


let let'<'s> : TokenParser<'s> = constToken "let" Let

let type'<'s> : TokenParser<'s> = constToken "type" Token.Type

let plus<'s> : TokenParser<'s> = constToken "+" Plus

let minus<'s> : TokenParser<'s> = constToken "-" Minus

let asterisk<'s> : TokenParser<'s> = constToken "*" Asterisk

let slash<'s> : TokenParser<'s> = constToken "/" Slash

let equals<'s> : TokenParser<'s> = constToken "=" Equals

let notEquals<'s> : TokenParser<'s> = constToken "/=" NotEquals

let parenOpen<'s> : TokenParser<'s> = constToken "(" ParenOpen

let parenClose<'s> : TokenParser<'s> = constToken ")" ParenClose

let hardBreak<'s> : TokenParser<'s> = constToken ";" HardBreak

let identifier<'s> : TokenParser<'s> =
    let firstCharCond c =
        Char.IsLetter c ||
        c = '_'

    let followingCharCond c =
        Char.IsLetterOrDigit c ||
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
    >> ParseResult.mapValue makeToken

let number<'s> : TokenParser<'s> =
    let integerPart = oneOrMoreCond Char.IsDigit
    let fractionalPart = optional (skipOne '.' >>. oneOrMoreCond Char.IsDigit)

    let makeToken (integerPart : char array, fractionalPart : char array option) =
        let integerPart = integerPart |> String |> UInt32.Parse
        let fractionalPart = fractionalPart |> Option.map (String >> UInt32.Parse)
        Number(integerPart, fractionalPart)

    integerPart .>>. fractionalPart
    >> ParseResult.mapValue makeToken

let quotedString<'s> : TokenParser<'s> =
    (skipOne '"' >>. zeroOrMoreAnyWithTerminator '"')
    >> ParseResult.mapValue (String >> QuotedString)

let invalidToken<'s> : TokenParser<'s> =
    oneOrMoreCond (fun c -> not (isWhiteSpace c) && c <> '\n' && c <> '\r')
    >> ParseResult.mapValue (String >> InvalidToken)
