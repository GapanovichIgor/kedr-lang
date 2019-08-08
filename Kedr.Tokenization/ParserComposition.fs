module internal Kedr.Tokenization.ParserComposition

let inline private combine valueSelector p1 p2 =
    fun (tape: Tape<_>, state) ->
        match p1 (tape, state) with
        | Ok ok1 ->
            match p2 (tape, ok1.state) with
            | Ok ok2 ->
                { value = valueSelector (ok1.value, ok2.value)
                  state = ok2.state
                  length = ok1.length + ok2.length }
                |> Ok
            | Error e ->
                tape.MoveBack(ok1.length)
                Error e
        | Error e -> Error e

let (>>.) (p1: Parser<'i, 's, _>) (p2: Parser<'i, 's, 'o>): Parser<'i, 's, 'o> =
    combine snd p1 p2

let (.>>) (p1: Parser<'i, 's, 'o>) (p2: Parser<'i, 's, _>): Parser<'i, 's, 'o> =
    combine fst p1 p2

let (.>>.) (p1: Parser<'i, 's, 'o1>) (p2: Parser<'i, 's, 'o2>): Parser<'i, 's, 'o1 * 'o2> =
    combine id p1 p2

let chooseFirstLongest (parsers: Parser<'i, 's, 'o> list): Parser<'i, 's, 'o> =
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

    fun (tape, state) ->
        let result =
            parsers
            |> Seq.map (fun p ->
                let r = p (tape, state)

                match r with
                | Ok s -> tape.MoveBack(s.length)
                | _ -> ()

                r)
            |> Seq.fold combine initial

        match result with
        | Ok s -> tape.MoveForward(s.length)
        | _ -> ()

        result

let optional (parser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o option> =
    fun (tape, state) ->
        match parser (tape, state) with
        | Ok o ->
            { value = Some o.value
              state = o.state
              length = o.length }
            |> Ok
        | Error _ ->
            { value = None
              state = state
              length = 0 }
            |> Ok

let zeroOrMore (parser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable state = state
        let results =
            [ while keepGoing do
                match parser (tape, state) with
                | Ok o ->
                    yield o
                    state <- o.state
                | Error _ -> keepGoing <- false ]

        { value = results |> List.map (fun s -> s.value)
          state = state
          length = results |> List.sumBy (fun s -> s.length) }
        |> Ok

let zeroOrMoreDelimited (delimiter: Parser<'i, 's, _>) (parser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable moreRequired = false
        let mutable error = None
        let mutable state = state
        let results =
            [
                while keepGoing do
                    match parser (tape, state) with
                    | Ok o ->
                        yield o
                        state <- o.state
                        match delimiter (tape, state) with
                        | Ok o ->
                            moreRequired <- true
                            state <- o.state
                        | _ -> keepGoing <- false
                    | Error e ->
                        if moreRequired then
                            error <- Some e
            ]
            
        match error with
        | Some e -> Error e
        | None ->
            { value = results |> List.map (fun s -> s.value)
              state = state
              length = results |> List.sumBy (fun s -> s.length) }
            |> Ok

let oneOrMore (parser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o list> =
    fun (tape, state) ->
        let mutable keepGoing = true
        let mutable error = None
        let mutable state = state
        let results =
            [ while keepGoing do
                  match parser (tape, state) with
                  | Ok o ->
                      yield o
                      state <- state
                  | e ->
                      error <- Some e
                      keepGoing <- false ]

        if results.Length > 0 then
            { value = results |> List.map (fun s -> s.value)
              state = state
              length = results |> List.sumBy (fun s -> s.length) }
            |> Ok
        else
            Error()

let orElse (fallbackParser: Parser<'i, 's, 'o>) (mainParser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o> =
    fun (tape, state) ->
        match mainParser (tape, state) with
        | Error _ -> fallbackParser (tape, state)
        | r -> r

let commitOnSuccess (parser: Parser<'i, 's, 'o>): Parser<'i, 's, 'o> =
    fun (tape, state) ->
        let r = parser (tape, state)
        match r with
        | Ok o ->
            tape.Commit(o.length)
        | _ -> ()

        r