module internal Kedr.ParserComposition

let inline private combine valueSelector p1 p2 =
    fun (tape: Tape<_>) ->
        match p1 tape with
        | Ok ok1 ->
            match p2 tape with
            | Ok ok2 ->
                { value = valueSelector (ok1.value, ok2.value)
                  length = ok1.length + ok2.length }
                |> Ok
            | Error e ->
                tape.MoveBack(ok1.length)
                Error e
        | Error e -> Error e

let (>>.) (p1: Parser<'i, _>) (p2: Parser<'i, 'o>): Parser<'i, 'o> =
    combine snd p1 p2

let (.>>) (p1: Parser<'i, 'o>) (p2: Parser<'i, _>): Parser<'i, 'o> =
    combine fst p1 p2

let (.>>.) (p1: Parser<'i, 'o1>) (p2: Parser<'i, 'o2>): Parser<'i, 'o1 * 'o2> =
    combine id p1 p2

let chooseFirstLongest (parsers: Parser<'i, 'o> list): Parser<'i, 'o> =
    assert (parsers.Length >= 2)

    let combine r1 r2 =
        match r1, r2 with
        | Ok s1, Ok s2 ->
            if s1.length < s2.length then
                Ok s2
            else
                Ok s1
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
        
//let chooseFirstToSucceed (parsers: Parser<'i, 'o> list): Parser<'i, 'o> =
//    assert(parsers.Length >= 2)
//    
//    fun tape ->
//        let result =
//            parsers
//            |> Seq.map (fun p -> p tape)
//            |> Seq.find (fun r -> match r with Ok _ -> true | Error _ -> false)
//            
//        result

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

let orElse (fallbackParser: Parser<'i, 'o>) (mainParser: Parser<'i, 'o>): Parser<'i, 'o> =
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
