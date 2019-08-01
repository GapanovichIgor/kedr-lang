namespace Kedr.Tokenizer.Tests.Data
open FsCheck

type AnyToken(tokenText, debugText, shrink) =
    member val tokenText = tokenText
    member val shrink = shrink

    override __.ToString() = debugText

module AnyToken =
    let (|AnyToken'|) (a : AnyToken) =
        a.tokenText

    let rec private fromNumber n =
        let shrink() =
            Number.shrink n
            |> Seq.map fromNumber

        let text = n.ToString()

        AnyToken(text, text, shrink)

    let rec private fromQuotedString q =
        let shrink() =
            QuotedString.shrink q
            |> Seq.map fromQuotedString

        AnyToken(q.tokenText, q.ToString(), shrink)

    let rec private fromIdentifier i =
        let shrink() =
            Identifier.shrink i
            |> Seq.map fromIdentifier

        let text = let (Identifier i) = i in i.ToString()

        AnyToken(text, text, shrink)

    let private constGen t =
        let at = AnyToken(t, t, fun () -> Seq.empty)

        at |> Gen.constant

    let gen =
        Gen.oneof [
            Number.gen |> Gen.map fromNumber
            QuotedString.gen |> Gen.map fromQuotedString
            Identifier.gen |> Gen.map fromIdentifier
            constGen "let"
            constGen "type"
            constGen "+"
            constGen "-"
            constGen "*"
            constGen "/"
            constGen "="
            constGen "/="
            constGen "("
            constGen ")"
        ]

    let shrink (at : AnyToken) = at.shrink()

    let arb = Arb.fromGenShrink (gen, shrink)
