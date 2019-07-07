module internal Kedr.ParserComposition

let inline private combine valueSelector p1 p2 =
    fun tape ->
        p1 tape |> Result.bind (fun ok1 ->
            p2 tape |> Result.bind (fun ok2 ->
                { value = valueSelector (ok1.value, ok2.value)
                  length = ok1.length + ok2.length }
                |> Ok))

let (>>.) (p1: Parser<'i, _>) (p2: Parser<'i, 'o>): Parser<'i, 'o> =
    combine snd p1 p2

let (.>>) (p1: Parser<'i, 'o>) (p2: Parser<'i, _>): Parser<'i, 'o> =
    combine fst p1 p2

let (.>>.) (p1: Parser<'i, 'o1>) (p2: Parser<'i, 'o2>): Parser<'i, 'o1 * 'o2> =
    combine id p1 p2

let chooseLongest (parsers: Parser<'i, 'o> list): Parser<'i, 'o> =
    assert (parsers.Length >= 2)
    
    let combine r1 r2 =
        match r1, r2 with
        | Ok s1, Ok s2 ->
            if s1.length > s2.length then
                Ok s1
            elif s1.length < s2.length then
                Ok s2
            else
                Error()
        | Error _, Ok s2 -> Ok s2
        | Ok s1, Error _ -> Ok s1
        | Error _, Error _ -> Error()

    let initial = Error()

    fun tape ->
        let result =
            parsers
            |> Seq.map (fun p ->
                let r = p tape
                
                match r with
                | Ok s -> tape.MoveBack(s.length)
                | _ -> ()
                
                r)
            |> Seq.fold combine initial
        
        match result with
        | Ok s -> tape.MoveForward(s.length)
        | _ -> ()
        
        result

let optional (parser: Parser<'i, 'o>): Parser<'i, 'o option> =
    fun tape ->
        match parser tape with
        | Ok o -> Ok { value = Some o.value; length = o.length }
        | Error _ -> Ok { value = None; length = 0 }

let zeroOrMore (parser: Parser<'i, 'o>): Parser<'i, 'o list> =
    fun tape ->
        let mutable keepGoing = true
        let results =
            [ while keepGoing do
                match parser tape with
                | Ok o -> yield o
                | Error _ -> keepGoing <- false ]

        { value = results |> List.map (fun s -> s.value)
          length = results |> List.sumBy (fun s -> s.length) }
        |> Ok
        
let orElse (fallbackParser : Parser<'i, 'o>) (mainParser: Parser<'i, 'o>) : Parser<'i, 'o> =
    fun tape ->
        match mainParser tape with
        | Error _ -> fallbackParser tape
        | r -> r

let commitOnSuccess (parser: Parser<'i, 'o>): Parser<'i, 'o> =
    fun tape ->
        let r = parser tape
        match r with
        | Ok o ->
            tape.Commit(o.length)
        | _ -> ()

        r
