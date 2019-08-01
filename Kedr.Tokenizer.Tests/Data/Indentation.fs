namespace Kedr.Tokenizer.Tests.Data
open System
open FsCheck
open Kedr.Tokenizer.Tests

[<StructuredFormatDisplay("{AsString}")>]
type Indentation = Indentation of string
    with
    member this.AsString = let (Indentation text) = this in Util.escapeString text

module Indentation =
    let gen = gen {
        let! char = Gen.elements [ ' '; '\t' ]
        
        let! list = Gen.nonEmptyListOf (Gen.constant char)
        
        return
            list
            |> List.toArray
            |> String
            |> Indentation
    }
    
    let shrink (Indentation text) =
        seq {
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