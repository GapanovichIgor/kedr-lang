module internal Kedr.ParserPrimitives
open System.Collections.Generic

let skip<'i when 'i : equality> (item : 'i) (tape : Tape<'i>) : ParseResult<unit> =
    tape.MoveNext()
    if tape.Current = Some item then
        Ok { value = (); length = 1 }
    else
        tape.Backtrack(1)
        Error ()
        
//let skipZeroOrMore2<'i when 'i : equality> (items : 'i list) (tape : Tape<'i>) : ParseResult<unit> =
//    let items = items |> HashSet
//    
//    let rec loop advanceCount =
//        tape.MoveNext()
//        let advanceCount = advanceCount + 1
//        match tape.Current with
//        | Some i when items.Contains(i) ->
//            loop (advanceCount)
//        | _ ->
//            tape.Backtrack(1)
//            Error ()
    
        
let anyWithTerminator<'i when 'i : equality> (item : 'i) (tape : Tape<'i>) : ParseResult<'i array> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when i = item ->
            let items = 
                if advanceCount > 1 then
                    tape.Backtrack(advanceCount)
                    let items = tape.Consume(advanceCount - 1)
                    tape.MoveNext()
                    items
                else
                    [||]
            Ok { value = items; length = advanceCount }
        | Some _ ->
            loop (advanceCount)
        | None ->
            tape.Backtrack(advanceCount)
            Error ()
        
    loop 0
    
let takeWhile<'i> (pred : 'i -> bool) (tape : Tape<'i>) : ParseResult<'i array> =
    let rec loop advanceCount =
        tape.MoveNext()
        let advanceCount = advanceCount + 1
        match tape.Current with
        | Some i when pred i ->
            loop (advanceCount)
        | _ ->
            tape.Backtrack(advanceCount)
            if advanceCount > 1 then
                let items = tape.Consume(advanceCount - 1) 
                Ok { value = items; length = items.Length }
            else
                Error ()
                
    loop 0
    
let skipWhile<'i> (pred : 'i -> bool) (tape : Tape<'i>) : ParseResult<unit> =
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