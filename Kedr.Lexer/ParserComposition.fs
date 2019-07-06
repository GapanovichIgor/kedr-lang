module internal Kedr.ParserComposition

let (>>.) (p1: Parser<'i, _>) (p2: Parser<'i, 'o>): Parser<'i, 'o> =
    fun tape -> p1 tape |> Result.bind (fun _ -> p2 tape)

let (.>>) (p1: Parser<'i, 'o>) (p2: Parser<'i, _>): Parser<'i, 'o> =
    fun tape -> p1 tape |> Result.bind (fun x -> p2 tape |> Result.map (fun _ -> x))

let combine (p2: Parser<'i, 'o2>) (combineOutput: 'o1 -> 'o2 -> 'o) (p1: Parser<'i, 'o1>): Parser<'i, 'o> =
    fun tape ->
        p1 tape |> Result.bind (fun o1 ->
            p2 tape |> Result.map (fun o2 ->
                { value = combineOutput o1.value o2.value
                  length = o1.length + o2.length }))

let chooseLongest (parsers: Parser<'i, 'o> list): Parser<'i, 'o> =
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
        let t =
            parsers
            |> Seq.map (fun p -> p tape)
        ()
        t
        |> Seq.fold combine initial

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

let commitOnSuccess (parser: Parser<'i, 'o>): Parser<'i, 'o> =
    fun tape ->
        let r = parser tape
        match r with
        | Ok o ->
            tape.Commit(o.length)
        | _ -> ()

        r