namespace Kedr.Tokenization.Tests.Data
open System
open FsCheck
open Kedr.Tokenization.Tests

type Indentation(text) =
    member val tokenText = text

    override __.ToString() = Util.escapeString text

module Indentation =
    let (|Indentation'|) (i : Indentation) = i.tokenText

    let gen = gen {
        let! char = Gen.elements [ ' '; '\t' ]

        let! list = Gen.nonEmptyListOf (Gen.constant char)

        return
            list
            |> List.toArray
            |> String
            |> Indentation
    }

    let shrink (i : Indentation) =
        seq {
            let text = i.tokenText
            if text.Length > 1 then
                yield text.Substring(1, 1)

                yield text.Substring(1)

                if text.[0] = '\t' then
                    yield new String(' ', text.Length)

                if text.Length > 1 then
                    yield text.Substring(0, text.Length - 1)
        }
        |> Seq.map Indentation

    let arb = Arb.fromGenShrink (gen, shrink)
