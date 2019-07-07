module Kedr.Lexer.Tests.TokenizerTests

open Expecto
open Kedr
open Kedr.Tokenizer
open Kedr.Lexer.Tests.Setup

let private parse = fun source -> parseString source

[<Tests>]
let tests =
    testList "tokenizer properties" [
        "number is parsed as such" :=
            fun (number: TestNumber) ->
                parse !number = [ Number(number.integerPart, number.fractionalPart) ]

        "quoted string is parsed as such" :=
            fun (strLit: TestQuotedString) ->
                parse !strLit = [ QuotedString strLit.contents ]

        "plus is parsed as such" :=
            parse "+" = [ Plus ]

        "minus is parsed as such" :=
            parse "-" = [ Minus ]

        "asterisk is parsed as such" :=
            parse "*" = [ Asterisk ]

        "slash is parsed as such" :=
            parse "/" = [ Slash ]

        "equals is parsed as such" :=
            parse "=" = [ Equals ]

        "not equals is parsed as such" :=
            parse "/=" = [ NotEquals ]

        "whitespace is not a token" :=
            fun (ws: TestWhitespace) ->
                parse !ws = []

        "whitespace identity" :=
            fun (ws: TestWhitespace) (token: TestAnyToken) ->
                parse (!ws + !token) = parse !token
                &&
                parse (!token + !ws) = parse !token

        "parsing token concatenation = parsing one by one" :=
            fun (ws: TestWhitespace) (tokens: TestAnyToken list) ->
                let tokenStrs = tokens |> List.map toStr

                let parsedTogether =
                    tokenStrs
                    |> String.concat !ws
                    |> parse

                let parsedIndividually =
                    tokenStrs
                    |> List.collect parse

                parsedTogether = parsedIndividually

        "invalid token is parsed as such" :=
            parse "." = [ InvalidToken "." ]
    ]
