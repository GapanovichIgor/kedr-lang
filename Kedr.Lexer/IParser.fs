namespace Kedr

type internal ParserStatus = Initial | PartialMatch | CompleteMatch | FailedMatch

type internal IParser<'i, 'o> =
    abstract Status : ParserStatus
    abstract Feed : 'i -> unit
    abstract Parse : 'i array -> 'o
    abstract Reset : unit -> unit