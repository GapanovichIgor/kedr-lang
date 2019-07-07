namespace Kedr.Lexer.Tests
open System
open FsCheck

type TestIdentifier(identifier : string) =
    
    member val str = identifier
    
    override this.ToString() = this.str
    
    static member generator = gen {
        let! length =
            Arb.generate<uint32>
            |> Gen.filter (fun l -> l > 0u)
        let length = int32 length
            
        let chars = Array.zeroCreate<char> length
        
        let! firstChar =
            Arb.generate<char>
            |> Gen.filter (fun c -> Char.IsLetter c || c = '_')
            
        chars.[0] <- firstChar
        
        for i = 1 to length - 1 do
            let! char =
                Arb.generate<char>
                |> Gen.filter (fun c ->
                    Char.IsLetter c ||
                    c = '_') // TODO other chars
                
            chars.[i] <- char
            
        return
            chars
            |> String
            |> TestIdentifier
    }
    
    static member shrink (value : TestIdentifier) = seq {
        if value.str.Length > 1 then
            yield value.str.Substring(0, value.str.Length - 1) |> TestIdentifier
    }
    
    static member arb = Arb.fromGenShrink (TestIdentifier.generator, TestIdentifier.shrink)