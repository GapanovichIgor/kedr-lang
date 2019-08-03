open Hedgehog
open Kedr
open Tokenizer
open Kedr.Tokenizer.Tests.Generators
open Kedr.Tokenizer.Tests.Utils

let private parse = fun source -> parseString source

[<EntryPoint>]
let main _ =
    [
        "number is parsed as such" := property {
            let! (text, i, f) = numberGen
            
            return parse text = [ Number(i, f) ]
        }

        "quoted string is parsed as such" := property {
            let! (text, content) = quotedStringGen
            
            return parse text = [ QuotedString content ]
        }

        "identifier is parsed as such" := property {
            let! identifier = identifierGen
            
            return parse identifier = [ Identifier identifier ]
        }

        "let is parsed as such" := property {
            return parse "let" = [ Let ]
        }

        "type is parsed as such" := property {
            return parse "type" = [ Type ]
        }

        "plus is parsed as such" := property {
            return parse "+" = [ Plus ]
        }

        "minus is parsed as such" := property {
            return parse "-" = [ Minus ]
        }

        "asterisk is parsed as such" := property {
            return parse "*" = [ Asterisk ]
        }

        "slash is parsed as such" := property {
            return parse "/" = [ Slash ]
        }

        "equals is parsed as such" := property {
            return parse "=" = [ Equals ]
        }

        "not equals is parsed as such" := property {
            return parse "/=" = [ NotEquals ]
        }

        "opening parenthesis is parsed as such" := property {
            return parse "(" = [ ParenOpen ]
        }

        "closing parenthesis is parsed as such" := property {
            return parse ")" = [ ParenClose ]
        }

        "whitespace is not a token" := property {
            let! ws = whitespaceGen
                
            return parse ws = []
        }

        "whitespace identity" := property {
            let! ws = whitespaceGen
            let! token = anyTokenGen
            
            return parse (ws + token) = parse token
            return parse (token + ws) = parse token
        }

        "parsing token concatenation = parsing one by one" := property {
            let! ws = whitespaceGen
            let! tokens = anyTokenGen |> Gen.list (Range.exponential 0 100)
            
            let parsedTogether =
                tokens
                |> String.concat ws
                |> parse

            let parsedIndividually =
                tokens
                |> List.collect parse

            return parsedTogether = parsedIndividually
        }

        "invalid token is parsed as such" := property {
            return parse "@" = [ InvalidToken "@" ]
        }

        "test" := property {
            return parse "let add x y = x + y" = [ Let; Identifier "add"; Identifier "x"; Identifier "y"; Equals; Identifier "x"; Plus; Identifier "y" ]
        }
    ]
//    |> List.iter printProperty
    let t =
        Gen.list (Range.exponentialBounded ()) (Gen.constant 0)
        |> Gen.sample 100 5
//    |> List.iter System.Console.WriteLine
    
    0