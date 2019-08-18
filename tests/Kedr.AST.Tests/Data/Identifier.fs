namespace Kedr.AST.Tests.Data
open FsCheck

type Identifier(text) =
    member val text = text

module IdentifierToken =
    let (|IdentifierToken'|) (i : Identifier) = i.text

    let gen = Arb.generate<string> |> Gen.map Identifier

    let shrink (i : Identifier) =
        Arb.shrink i.text
        |> Seq.map Identifier

    let arb = Arb.fromGenShrink (gen, shrink)
