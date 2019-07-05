namespace Kedr
open System

type internal MatcherStatus = Initial | PartialMatch | CompleteMatch | FailedMatch

type internal IParser<'i, 'o> =
    abstract Status : MatcherStatus
    abstract Feed : 'i -> unit
    abstract Parse : 'i array -> 'o
    abstract Reset : unit -> unit