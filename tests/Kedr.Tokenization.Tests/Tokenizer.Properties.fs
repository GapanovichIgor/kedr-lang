[<FsCheck.Xunit.Properties(Arbitrary = [| typeof<Kedr.Tokenization.Tests.Arbs> |])>]
module Kedr.Tokenization.Tests.Tokenizer.Properties

open System
open FsCheck
open FsCheck.Xunit
open Kedr.Tokenization
open Kedr.Tokenization.Tests.Util
open Kedr.Tokenization.Tests.Data
open Kedr.Tokenization.Tests.Data.Number
open Kedr.Tokenization.Tests.Data.Whitespace
open Kedr.Tokenization.Tests.Data.Identifier
open Kedr.Tokenization.Tests.Data.Indentation
open Kedr.Tokenization.Tests.Data.QuotedString
open Kedr.Tokenization.Tests.Data.NewLine
open Kedr.Tokenization.Tests.Data.AnyToken

type private T = Token

let private parse source =
    let parseResult = Tokenizer.parseString source
    parseResult.tokens

let [<Property>] ``number is parsed as such``
    (Number''(text, i, f)) =

    parse text == [ T.Number(i, f) ]

let [<Property>] ``quoted string is parsed as such``
    (QuotedString''(text, content)) =

    parse text == [ T.QuotedString content ]

let [<Property>] ``identifier is parsed as such``
    (Identifier' i) =

    parse i == [ T.Identifier i ]

let [<PropertyOnce>] ``let is parsed as such``
    () =

    parse "let" == [ T.Let ]

let [<PropertyOnce>] ``type is parsed as such``
    () =

    parse "type" == [ T.Type ]

let [<PropertyOnce>] ``module is parsed as such``
    () =

    parse "module" == [ T.Module ]

let [<PropertyOnce>] ``plus is parsed as such``
    () =

    parse "+" == [ T.Plus ]

let [<PropertyOnce>] ``minus is parsed as such``
    () =

    parse "-" == [ T.Minus ]

let [<PropertyOnce>] ``asterisk is parsed as such``
    () =

    parse "*" == [ T.Asterisk ]

let [<PropertyOnce>] ``slash is parsed as such``
    () =

    parse "/" == [ T.Slash ]

let [<PropertyOnce>] ``equals is parsed as such``
    () =

    parse "=" == [ T.Equals ]

let [<PropertyOnce>] ``not equals is parsed as such``
    () =

    parse "/=" == [ T.NotEquals ]

let [<PropertyOnce>] ``colon is parsed as such``
    () =

    parse ":" == [ T.Colon ]

let [<PropertyOnce>] ``opening parenthesis is parsed as such``
    () =

    parse "(" == [ T.ParenOpen ]

let [<PropertyOnce>] ``closing parenthesis is parsed as such``
    () =

    parse ")" == [ T.ParenClose ]

let [<PropertyOnce>] ``square bracket open is parsed as such``
    () =

    parse "[" == [ T.SquareBracketOpen ]

let [<PropertyOnce>] ``square bracket close is parsed as such``
    () =

    parse "]" == [ T.SquareBracketClose ]

let [<PropertyOnce>] ``hard break is parsed as such``
    () =

    parse ";" == [ T.HardBreak ]

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

let [<PropertyOnce>] ``invalid token is parsed as such``
    () =

    parse "@" == [ T.InvalidToken "@" ]

let [<Property>] ``indentation increase creates a block``
    (Indentation' ind)
    (NewLine' nl)
    (AnyToken' tok1)
    (AnyToken' tok2) =

    parse (ind + tok1 + nl + ind + ind + tok2) ==
        (parse tok1) @ [ T.BlockOpen ] @ (parse tok2) @ [ T.BlockClose ]

let [<Property>] ``no indentation is level 0 indentation``
    (Indentation' ind)
    (NewLine' nl)
    (AnyToken' tok1)
    (AnyToken' tok2) =

    parse (tok1 + nl + ind + tok2) == parse (ind + tok1 + nl + ind + ind + tok2)

let [<Property>] ``number of block opens is always the same as the number of block closes``
    (lines : (Indentation * AnyToken list) list) =

    let text =
        lines
        |> List.map (fun ((Indentation' ind), tokens) ->
            let linePayload =
                tokens
                |> List.map (fun (AnyToken' tok) -> tok)
                |> String.Concat
            ind + linePayload)
        |> String.Concat

    let tokens = parse text

    let count tok = tokens |> Seq.filter (fun tok' -> tok' = tok) |> Seq.length

    count T.BlockOpen == count T.BlockClose

let [<Property>] ``keeping indentation level produces soft break``
    (Indentation' ind)
    (AnyToken' tok1)
    (AnyToken' tok2)
    (NewLine' nl) =

    parse (ind + tok1 + nl + ind + tok2) == (parse tok1) @ [ T.SoftBreak ] @ (parse tok2)

let [<Property>] ``empty or whitespace-only line does not affect block formation``
    (Indentation' ind1)
    (Indentation' ind2)
    (AnyToken' tok1)
    (Whitespace' ws)
    (AnyToken' tok2)
    (NewLine' nl) =

    let expectation = (parse tok1) @ [ T.SoftBreak ] @ (parse tok2)

    (parse (ind1 + tok1 + nl + ind2 + ws + nl + ind1 + tok2) == expectation) .&.
    (parse (ind1 + tok1 + nl + ind2      + nl + ind1 + tok2) == expectation) .&.
    (parse (ind1 + tok1 + nl             + nl + ind1 + tok2) == expectation)

let [<PropertyOnce>] ``arbitrary example test``
    () =

    parse "let add x y = x + y;" ==
        [ T.Let
          T.Identifier "add"
          T.Identifier "x"
          T.Identifier "y"
          T.Equals
          T.Identifier "x"
          T.Plus
          T.Identifier "y"
          T.HardBreak ]
