module Kedr.Lexer.Tests.ParserTests

open Expecto
open Kedr

let private sut = fun source -> Parser.parseString ("source.kedr", source)

[<Tests>]
let tests =
    testList "" [
        testCase "parsed integer number" <| fun _ ->
            let result = sut "1"
            
            let expected =
                Parser.Token.Number "1"
                |> List.singleton
                |> Ok
                
            Expect.equal result expected ""
            
        testCase "parsed floating point number" <| fun _ ->
            let result = sut "1.0"
            
            let expected =
                Parser.Token.Number "1.0"
                |> List.singleton
                |> Ok
                
            Expect.equal result expected ""
    ]