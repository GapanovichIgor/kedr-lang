namespace Kedr.Tokenizer.Tests.Data
open System
open FsCheck
open Kedr.Tokenizer.Tests

type Whitespace(text) =
    member val tokenText = text

    override __.ToString() = Util.escapeString text

module Whitespace =
    let (|Whitespace'|) (ws : Whitespace) = ws.tokenText

    let gen =
        Gen.elements [ ' '; '\t' ]
        |> Gen.nonEmptyListOf
        |> Gen.map (List.toArray >> String >> Whitespace)

    let shrink (ws : Whitespace) =
        seq {
            let text = ws.tokenText
            if text.Length > 1 then
                yield text.Substring(1, 1)

                yield text.Substring(1)

                let firstTabIndex = text.IndexOf('\t')
                if firstTabIndex <> -1 then
                    yield text.Replace("\t", " ")

                if text.Length > 2 then
                    yield text.Substring(0, text.Length - 1)
            elif text = "\t" then
                yield " "
        }
        |> Seq.map Whitespace

    let arb = Arb.fromGenShrink (gen, shrink)
