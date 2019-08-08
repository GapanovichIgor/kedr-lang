namespace Kedr.Tokenizer.Tests.Data
open FsCheck
open Kedr.Tokenizer.Tests

type NewLine(text) =
    member val tokenText = text

    override __.ToString() = Util.escapeString text

module NewLine =
    let (|NewLine'|) (nl : NewLine) = nl.tokenText
    
    let gen =
        Gen.elements [ "\n"; "\r"; "\r\n" ]
        |> Gen.map NewLine

    let shrink (nl : NewLine) =
        seq {
            let text = nl.tokenText
            if text = "\r\n" then
                yield "\n"
                yield "\r"
            elif text = "\r" then
                yield "\n"
        }
        |> Seq.map NewLine

    let arb = Arb.fromGenShrink (gen, shrink)
