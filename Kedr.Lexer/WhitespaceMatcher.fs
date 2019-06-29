namespace Kedr.Parser

type internal WhitespaceMatcher() =
    let mutable status = Initial

    interface IMatcher with
        override val Parser = None
        override __.Reset() =
            status <- Initial
        override __.Status = status
        override __.Feed(c) =
            status <-
                match c with
                | ' ' | '\t' -> CompleteMatch
                | _ -> FailedMatch