namespace Kedr.Tokenizer.Tests.Data
open FsCheck
open Kedr.Tokenizer.Tests

[<StructuredFormatDisplay("{AsString}")>]
type NewLine = NewLine of string
    with
    member this.AsString = let (NewLine text) = this in Util.escapeString text

module NewLine =
    let gen =
        Gen.elements [ "\n"; "\r"; "\r\n" ]
        |> Gen.map NewLine
    
    let shrink (NewLine text) =
        seq {
            if text = "\r\n" then
                yield "\n"
                yield "\r"
            elif text = "\r" then
                yield "\n"
        }
        |> Seq.map NewLine
        
    let arb = Arb.fromGenShrink (gen, shrink)