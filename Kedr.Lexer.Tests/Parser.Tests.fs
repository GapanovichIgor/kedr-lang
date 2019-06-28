module Kedr.Lexer.Tests.ParserTests

open Expecto
open Kedr.Parser
open Kedr.Lexer.Tests.Setup
type Token = Kedr.Lexer.Tests.Token

let private parse = fun source -> parseString ("source.kedr", source)

[<Tests>]
let tests =
    testList "properties" [
        "decimal number literal is parsed back to itself" :=
            fun (decNum : DecimalNumberLiteral) ->
                parse !decNum = Ok [ Number !decNum ]

        "string literal is parsed back to its contents" :=
            fun (strLit : StringLiteral) ->
                let t1 = parse !strLit
                let t2 = parse !strLit = Ok [ StringLiteral strLit.contents ]

                if not t2 then
                    ()

                parse !strLit = Ok [ StringLiteral strLit.contents ]

        "whitespace is separator" :=
            fun (token1 : Token) (ws : Whitespace) (token2 : Token) ->
                parse (!token1 + !ws + !token2) = (parse !token1) ++ (parse !token2)
                &&
                parse (!token1 + !token2) <> (parse !token1) ++ (parse !token2)

        "whitespace left identity" :=
            fun (ws : Whitespace) (token : Token) ->
                parse (!ws + !token) = (parse !token)

        "whitespace right identity" :=
            fun (token : Token) (ws : Whitespace) ->
                let t = parse (!token + !ws)
                parse (!token + !ws) = (parse !token)
    ]
