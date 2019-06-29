namespace Kedr.Parser

type private State = Nothing | AtContents | AtEnd

type internal StringLiteralMatcher() =
    let ssNothing = (Nothing, Initial)
    let ssAtContents = (AtContents, PartialMatch)
    let ssAtEnd = (AtEnd, CompleteMatch)

    let mutable stateStatus = ssNothing

    interface IMatcher with
        override val Parser = Some StringLiteral
        override __.Status = snd stateStatus
        override __.Reset() = stateStatus <- ssNothing
        override __.Feed(c) =
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