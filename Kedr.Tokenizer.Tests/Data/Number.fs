namespace Kedr.Tokenizer.Tests.Data
open FsCheck

type Number =
    { integerPart: uint32
      fractionalPart: uint32 option }
    with
    override this.ToString() =
        match this.fractionalPart with
        | Some f -> sprintf "%u.%u" this.integerPart f
        | None -> sprintf "%u" this.integerPart
    
module Number =
    
    let (|Number''|) n =
        (n.ToString(), n.integerPart, n.fractionalPart)
        
    let (|Number'|) n = n.ToString()
    
    let gen =
        Gen.zip
            Arb.generate<uint32>
            (Arb.generate<uint32> |> Gen.optionOf)
        |> Gen.map (fun (i, f) -> { integerPart = i; fractionalPart = f })
        
    let shrink { integerPart = i; fractionalPart = f } =
        seq {
            match f with
            | Some f ->
                yield { integerPart = i; fractionalPart = None }
                
                yield!
                    Arb.shrink f
                    |> Seq.map (fun f -> { integerPart = i; fractionalPart = Some f })
                    
                yield!
                    Arb.shrink i
                    |> Seq.map (fun i -> { integerPart = i; fractionalPart = Some f })
            | None ->
                yield!
                    Arb.shrink i
                    |> Seq.map (fun i -> { integerPart = i; fractionalPart = None })
        }
        
    let arb = Arb.fromGenShrink (gen, shrink)