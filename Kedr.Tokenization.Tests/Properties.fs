namespace Kedr.Tokenization.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Kedr.Tokenization
open Kedr.Tokenization.Tests.Data
open Util
open Data.Number
open Data.Whitespace
open Data.Identifier
open Data.Indentation
open Data.QuotedString
open Data.NewLine
open Data.AnyToken

type private TK = Token

[<Properties(Arbitrary = [| typeof<Arbs> |])>]
module Properties =
    let private parse = fun source -> Tokenizer.parseString source

    let [<Property>] ``number is parsed as such``
        (Number''(text, i, f)) =

        parse text == [ TK.Number(i, f) ]

    let [<Property>] ``quoted string is parsed as such``
        (QuotedString''(text, content)) =

        parse text == [ TK.QuotedString content ]

    let [<Property>] ``identifier is parsed as such``
        (Identifier' i) =

        parse i == [ TK.Identifier i ]

    let [<Fact>] ``let is parsed as such``
        () =

        parse "let" == [ TK.Let ]

    let [<Fact>] ``type is parsed as such``
        () =

        parse "type" == [ TK.Type ]

    let [<Fact>] ``plus is parsed as such``
        () =

        parse "+" == [ TK.Plus ]

    let [<Fact>] ``minus is parsed as such``
        () =

        parse "-" == [ TK.Minus ]

    let [<Fact>] ``asterisk is parsed as such``
        () =

        parse "*" == [ TK.Asterisk ]

    let [<Fact>] ``slash is parsed as such``
        () =

        parse "/" == [ TK.Slash ]

    let [<Fact>] ``equals is parsed as such``
        () =

        parse "=" == [ TK.Equals ]

    let [<Fact>] ``not equals is parsed as such``
        () =

        parse "/=" == [ TK.NotEquals ]

    let [<Fact>] ``opening parenthesis is parsed as such``
        () =

        parse "(" == [ TK.ParenOpen ]

    let [<Fact>] ``closing parenthesis is parsed as such``
        () =

        parse ")" == [ TK.ParenClose ]
        
    let [<Fact>] ``hard break is parsed as such``
        () =
        
        parse ";" == [ TK.HardBreak ]

    let [<Property>] ``whitespace is not a token``
        (Whitespace' ws) =

        parse ws == []

    let [<Property>] ``whitespace identity``
        (Whitespace' ws)
        (AnyToken' t) =

        (parse (ws + t) == parse t) .&.
        (parse (t + ws) == parse t)

    let [<Property>] ``parsing token concatenation = parsing one by one``
        (Whitespace' ws)
        (tokens : AnyToken list) =

        let tokenStrs = tokens |> List.map (fun (AnyToken' t) -> t)

        let parsedTogether =
            tokenStrs
            |> String.concat ws
            |> parse

        let parsedIndividually =
            tokenStrs
            |> List.collect parse

        parsedTogether == parsedIndividually

    let [<Fact>] ``invalid token is parsed as such``
        () =

        parse "@" == [ TK.InvalidToken "@" ]

    let [<Property>] ``indentation increase creates a block``
        (Indentation' ind)
        (NewLine' nl)
        (AnyToken' tok1)
        (AnyToken' tok2) =
        
        parse (ind + tok1 + nl + ind + ind + tok2) ==
            (parse tok1) @ [ TK.BlockOpen ] @ (parse tok2) @ [ TK.BlockClose ]

    let [<Property>] ``no indentation is level 0 indentation``
        (Indentation' ind)
        (NewLine' nl)
        (AnyToken' tok1)
        (AnyToken' tok2) =
        
        parse (tok1 + nl + ind + tok2) == parse (ind + tok1 + nl + ind + ind + tok2)
        
    let [<Property>] ``block opens are always exactly matched by block closes``
        () =
        
        false
        
    let [<Property>] ``keeping indentation level produces soft break``
        (Indentation' ind)
        (AnyToken' tok1)
        (AnyToken' tok2)
        (NewLine' nl) =
        
        parse (ind + tok1 + nl + ind + tok2) == (parse tok1) @ [ TK.SoftBreak ] @ (parse tok2)
        
    let [<Property>] ``empty line does not affect block formation``
        (Indentation' ind1)
        (Indentation' ind2)
        (AnyToken' tok1)
        (Whitespace' ws)
        (AnyToken' tok2)
        (NewLine' nl) =
        
        parse (ind1 + tok1 + nl + ind2 + ws + nl + ind1 + tok2) == (parse tok1) @ [ TK.SoftBreak ] @ (parse tok2)

    let [<Fact>] ``arbitrary example test``
        () =

        parse "let add x y = x + y;" ==
            [ TK.Let
              TK.Identifier "add"
              TK.Identifier "x"
              TK.Identifier "y"
              TK.Equals
              TK.Identifier "x"
              TK.Plus
              TK.Identifier "y"
              TK.HardBreak ]
