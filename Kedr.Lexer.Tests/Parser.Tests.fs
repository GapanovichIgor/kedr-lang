module Kedr.Lexer.Tests.ParserTests

open Expecto
open Kedr
open Kedr.Tokenizer
open Kedr.Lexer.Tests.Setup

let private parse = fun source -> parseString source

[<Tests>]
let tests =
    testList "properties" [
        "decimal number literal is parsed back to itself" :=
            fun (decNum : TestNumber) ->
                parse !decNum = [ Number (decNum.integerPart, decNum.fractionalPart) ]

        "string literal is parsed back to its contents" :=
            fun (strLit : TestQuotedString) ->
                parse !strLit = [ QuotedString strLit.contents ]

        "whitespace is not a token" :=
            fun (ws : TestWhitespace) ->
                parse !ws = []
        
        "whitespace identity" :=
            fun (ws : TestWhitespace) (token : TestAnyToken) ->
                parse (!ws + !token) = parse !token
                &&
                parse (!token + !ws) = parse !token
                
        "parsing token concatenation = parsing one by one" :=
            fun (ws : TestWhitespace) (tokens : TestAnyToken list) ->
                let tokenStrs = tokens |> List.map toStr
                
                let parsedTogether =
                    tokenStrs
                    |> String.concat !ws
                    |> parse
                
                let parsedIndividually =
                    tokenStrs
                    |> List.collect parse
                    
                parsedTogether = parsedIndividually
    ]
