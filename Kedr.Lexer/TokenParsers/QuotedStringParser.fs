namespace Kedr
open System

type private QuotedStringParserState = Nothing | AtContents | AtEnd

type internal QuotedStringParser() =
    let ssNothing = (Nothing, Initial)
    let ssAtContents = (AtContents, PartialMatch)
    let ssAtEnd = (AtEnd, CompleteMatch)

    let mutable stateStatus = ssNothing

    interface IParser<char, Token> with
        override __.Parse (chars : char array) =
            String(chars, 1, chars.Length - 2)
            |> QuotedString
                
        override __.Status = snd stateStatus
        
        override __.Reset() = stateStatus <- ssNothing
        
        override __.Feed(c) =
            assert(snd stateStatus <> FailedMatch)
            
            let state = fst stateStatus
            stateStatus <-
                match state with
                | Nothing ->
                    if c = '"' then ssAtContents
                    else (state, FailedMatch)
                | AtContents ->
                    if c = '"' then ssAtEnd
                    else ssAtContents
                | AtEnd ->
                    (state, FailedMatch)