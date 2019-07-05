namespace Kedr

type internal LongestWinsCombinator<'i, 'o>(parsers : IParser<'i,'o> list) =
    let mutable aliveParsers = parsers
    
    let mutable status = Initial
    
    let mutable candidate : IParser<_,_> option = None
    
    interface IParser<'i, 'o> with
        override __.Status = status
        
        override __.Reset () =
            for parser in parsers do
                parser.Reset()
                
            aliveParsers <- parsers
            status <- Initial
            candidate <- None
        
        override __.Parse(sequence) =
            assert (candidate.IsSome)
            candidate.Value.Parse(sequence)
            
        override __.Feed(input) =
            assert(status <> FailedMatch)
            
            for parser in aliveParsers do
                parser.Feed(input)
                
            aliveParsers <- aliveParsers |> List.filter (fun p -> p.Status <> FailedMatch)
            
            if aliveParsers.Length = 0 then
                status <- FailedMatch
            else
                let newCandidates = aliveParsers |> List.filter (fun p -> p.Status = CompleteMatch)
                
                assert (newCandidates.Length <= 1)
                
                if newCandidates.Length = 1 then
                    candidate <- Some newCandidates.Head
                    status <- CompleteMatch
                else
                    status <- PartialMatch