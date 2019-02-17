module Kedr.Lexer.Tests.ParserTests

open Expecto
open Kedr

let sut = fun source -> Parser.parseString ("source.kedr", source)

[<Tests>]
let tests =
    testList "" [
        testCase "parsed digital only number" <| fun _ ->
            let result = sut "1"
            
            let expected =
                Parser.Token.Number "1"
                |> List.singleton
                |> Ok
                
            Expect.equal result expected ""
    ]