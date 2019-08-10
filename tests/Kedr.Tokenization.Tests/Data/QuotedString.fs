namespace Kedr.Tokenization.Tests.Data
open FsCheck
open Kedr.Tokenization.Tests
open System

type QuotedString(content) =
    member val content = content
    
    member val tokenText = sprintf "\"%s\"" content

    override __.ToString() =
        sprintf "\"%s\"" (Util.escapeString content)

module QuotedString =
    let (|QuotedString''|) (q : QuotedString) =
        (q.tokenText, q.content)

    let (|QuotedString'|) (q : QuotedString) =
        q.tokenText

    let gen =
        Arb.generate<char>
        |> Gen.filter ((<>) '"')
        |> Gen.nonEmptyListOf
        |> Gen.map (List.toArray >> String >> QuotedString)

    let shrink (qs : QuotedString) =
        Arb.shrink qs.content
        |> Seq.map QuotedString

    let arb = Arb.fromGenShrink (gen, shrink)
