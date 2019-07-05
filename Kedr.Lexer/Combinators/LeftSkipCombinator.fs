namespace Kedr
open System.Collections.Generic

type internal LeftSkipCombinator<'i, 'o>(toSkip : 'i list, parser : IParser<'i, 'o>) =
    let toSkip = toSkip |> HashSet
    
    let mutable status = Initial
    let mutable skippedCount = 0
    
    interface IParser<'i, 'o> with
        override __.Status = status
        
        override __.Reset() =
            parser.Reset()
            status <- Initial
            skippedCount <- 0
            
        override __.Parse(sequence) =
            sequence
            |> Array.skip skippedCount
            |> parser.Parse
            
        override __.Feed(input) =
            if toSkip.Contains(input) then
                skippedCount