namespace Kedr.Parser

type internal MatcherStatus = Initial | PartialMatch | CompleteMatch | FailedMatch

type internal IMatcher =
    abstract Status : MatcherStatus
    abstract Feed : char -> unit
    abstract Parser : (string -> Token) option
    abstract Reset : unit -> unit