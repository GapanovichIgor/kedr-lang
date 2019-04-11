module Kedr.Lexer.Tests.ParserTests

open Expecto
open Kedr.Parser
type Token = Kedr.Lexer.Tests.Token

let private parse = fun source -> parseString ("source.kedr", source)

type Arbs =
    static member DecimalNumber () = DecimalNumber.arb
    static member Token () = Token.arb
    static member Whitespace () = Whitespace.arb

let inline (++) res1 res2 =
    match res1, res2 with
    | Ok tokens1, Ok tokens2 -> Ok (tokens1 @ tokens2)
    | er, Ok _
    | Ok _, er -> er
    | Error reason1, Error reason2 -> Error (reason1 + "|" + reason2)

let inline (!) x =
    ( ^a : (member str : string with get) x )

[<Tests>]
let tests =
    let inline testProperty name f =
        let cfg = {
            FsCheckConfig.defaultConfig with
                arbitrary = typeof<Arbs> :: FsCheckConfig.defaultConfig.arbitrary
        }

        testPropertyWithConfig cfg name f

    testList "" [
        testProperty "a number is parsed back to itself" <| fun (decNum : DecimalNumber) ->
            parse !decNum = Ok [ Number !decNum ]

        testProperty "whitespace is separator" <|
            fun (token1 : Token) (ws : Whitespace) (token2 : Token) ->
                parse (!token1 + !ws + !token2) = (parse !token1) ++ (parse !token2)
                &&
                parse (!token1 + !token2) <> (parse !token1) ++ (parse !token2)

        testProperty "whitespace left identity" <|
            fun (ws : Whitespace) (token : Token) ->
                parse (!ws + !token) = (parse !token)

        testProperty "whitespace right identity" <|
            fun (ws : Whitespace) (token : Token) ->
                let t = parse (!token + !ws)
                parse (!token + !ws) = (parse !token)
    ]
