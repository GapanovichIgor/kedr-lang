namespace Kedr.Tokenizer.Tests.Data
open System
open FsCheck
open Kedr.Tokenizer.Tests

[<StructuredFormatDisplay("{AsString}")>]
type Whitespace = Whitespace of string
    with
    member this.AsString = let (Whitespace text) = this in Util.escapeString text

module Whitespace =
    let gen =
        Gen.elements [ ' '; '\t' ]
        |> Gen.nonEmptyListOf
        |> Gen.map (List.toArray >> String >> Whitespace)
    
    let shrink (Whitespace text) =
        seq {
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