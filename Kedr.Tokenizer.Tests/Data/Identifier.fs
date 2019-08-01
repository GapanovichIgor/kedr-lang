namespace Kedr.Tokenizer.Tests.Data
open System
open FsCheck

[<StructuredFormatDisplay("{AsString}")>]
type Identifier = Identifier of string
    with
    member this.AsString = let (Identifier text) = this in text

module Identifier =
    let private firstLetterGen =
        Gen.oneof [
            Gen.constant '_'
            Arb.generate<char> |> Gen.filter Char.IsLetter
        ]
        
    let private nonFirstLetterGen =
        Gen.oneof [
            Gen.constant '_'
            Arb.generate<char> |> Gen.filter Char.IsLetterOrDigit
        ]
    
    let gen =
        Gen.zip
            firstLetterGen
            (nonFirstLetterGen |> Gen.arrayOf |> Gen.map String)
        |> Gen.map (fun (l, ls) ->
            (string l) + ls
            |> Identifier)
        
    let shrink (Identifier text) =
        seq {
            if text.Length > 1 then
                yield text.Substring(0, 1)
                
                if text.Length > 2 then
                    yield text.Substring(0, 1) + text.Substring(1, text.Length - 2)
                
                yield text.Substring(0, 1) + text.Substring(2)
        }
        |> Seq.map Identifier
        
    let arb = Arb.fromGenShrink (gen, shrink)