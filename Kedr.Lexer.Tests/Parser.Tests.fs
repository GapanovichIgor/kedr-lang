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

//        "whitespace is separator" :=
//            fun () ->
//                let token1 = DecimalNumberLiteral(false, ) 
//                
//                parse (!token1 + !ws + !token2) = (parse !token1) ++ (parse !token2)
//                &&
//                parse (!token1 + !token2) <> (parse !token1) ++ (parse !token2)
//        "whitespace is separator" :=
//            fun () ->
//                let token1 = TestNumberLiteral("8", "6")
//                let token2 = TestStringLiteral("")
//                let ws = TestWhitespace("\t")
////                let t = parse !token1
////                let t2 = parse !token2
//                let t3 = parse (!token1 + !token2)
//                false
                
//        "whitespace is separator" :=
//            fun (token1 : TestToken) (ws : TestWhitespace) (token2 : TestToken) ->
//                parse (!token1 + !ws + !token2) = (parse !token1) ++ (parse !token2)
//                &&
//                parse (!token1 + !token2) <> (parse !token1) ++ (parse !token2)

        "whitespace is not a token" :=
            fun (ws : TestWhitespace) ->
                let t = parse !ws 
                parse !ws = []
        
//        "whitespace identity" :=
//            fun (ws : TestWhitespace) (token : TestAnyToken) ->
//                parse (!ws + !token) = parse !token
//                &&
//                parse (!token + !ws) = parse !token
//                
//        "parsing token concatenation = parsing one by one" :=
//            fun (ws : TestWhitespace) (tokens : TestAnyToken list) ->
//                let tokenStrs = tokens |> List.map toStr
//                
//                let parsedTogether =
//                    tokenStrs
//                    |> String.concat !ws
//                    |> parse
//                
//                let parsedIndividually =
//                    tokenStrs
//                    |> List.collect parse
//                    
//                parsedTogether = parsedIndividually
    ]
