module internal Kedr.ParserPrimitives

open ParserComposition

let skipAny<'i> (count: int) (tape: Tape<'i>): ParseResult<unit> =
    assert (count > 0)

    let rec loop i =
        if i = count then
            Ok { value = (); length = count }
        else
            tape.MoveNext()
            let i = i + 1
            if tape.Current.IsSome then
                loop i
            else
                tape.Backtrack(i)
                Error()

    loop 0

let skipOne<'i when 'i: equality> (item: 'i) (tape: Tape<'i>): ParseResult<unit> =
    tape.MoveNext()
    if tape.Current = Some item then
        Ok { value = (); length = 1 }
    else
        tape.Backtrack(1)
        Error()

let zeroOrMoreCond<'i> (cond: 'i -> bool) (tape: Tape<'i>): ParseResult<'i array> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when cond i ->
            loop advanceCount
        | _ ->
            tape.Backtrack(advanceCount)
            let items = tape.Consume(advanceCount - 1)
            Ok { value = items; length = items.Length }
    
    loop 0

let skipZeroOrMoreCond<'i> (pred: 'i -> bool) (tape: Tape<'i>): ParseResult<unit> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when pred i ->
            loop (advanceCount)
        | _ ->
            tape.Backtrack(1)
            Ok { value = (); length = advanceCount - 1 }

    loop 0

let oneOrMoreCond<'i> (cond: 'i -> bool) (tape: Tape<'i>): ParseResult<'i array> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when cond i ->
            loop advanceCount
        | _ ->
            tape.Backtrack(advanceCount)
            if advanceCount > 1 then
                let items = tape.Consume(advanceCount - 1)
                Ok { value = items; length = items.Length }
            else
                Error()

    loop 0

let zeroOrMoreAnyWithTerminator<'i when 'i: equality> (terminator: 'i) : Parser<'i, 'i array> =
    zeroOrMoreCond ((<>) terminator)
    .>>
    skipOne terminator
