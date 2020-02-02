module Kedr.ParserGenerator.LALRGenerator

let private createAugmentedGrammar
    (lr0Automaton : LR0Automaton)
    (productions : Production Set)
    : Production Set =

    let nonTerminals =
        productions
        |> Seq.map (fun p -> p.from)
        |> Set.ofSeq

    let states =
        seq {
            for tr in lr0Automaton.transitions do
                yield tr.sourceState
                yield tr.destinationState
        } |> Set.ofSeq

    let stateNumbers =
        states
        |> Seq.mapi (fun i state -> (state, i))
        |> Map.ofSeq

    let tryTransition state symbol : State option =
        let transitions =
            lr0Automaton.transitions
            |> Seq.filter (fun tr ->
                tr.sourceState = state &&
                tr.symbol = symbol)
            |> List.ofSeq
        match transitions with
        | [] -> None
        | [tr] -> Some tr.destinationState
        | _ -> failwith "LR0 contains reduce/reduce conflict"

    let transition state symbol = tryTransition state symbol |> Option.get

    let createAugmentedSymbol symbol stateFrom stateInto =
        let (Symbol symbolName) = symbol
        let stateFromNum = stateNumbers.[stateFrom]
        let stateIntoNum = stateNumbers.[stateInto]
        Symbol (sprintf "%s_%i_%i" symbolName stateFromNum stateIntoNum)

    let rec traceAndAugmentSymbols state remainingSymbols reverseResult =
        match remainingSymbols with
        | [] -> reverseResult |> List.rev
        | symbol :: symbolsRest ->
            let state' = transition state symbol
            let symbol' =
                if nonTerminals |> Set.contains symbol
                then createAugmentedSymbol symbol state state'
                else symbol

            let reverseResult = symbol' :: reverseResult

            traceAndAugmentSymbols state' symbolsRest reverseResult

    let productions' =
        seq {
            for state in states do
                let startConfigs = state.configurations |> Set.filter (fun cfg -> cfg.cursorOffset = 0)
                for config in startConfigs do
                    // config: A -> . w
                    let A = config.production.from
                    let w = config.production.into

                    let transitionStateOnA = tryTransition state A

                    let A' =
                        match transitionStateOnA with
                        | None -> A
                        | Some trState -> createAugmentedSymbol A state trState

                    let w' = traceAndAugmentSymbols state w []

                    yield {
                        from = A'
                        into = w'
                    }
        } |> Set.ofSeq

    productions'

let generate (productions : Production Set) =

    let lr0Automaton = LR0Automaton.generate productions

    let augmentedProductions = createAugmentedGrammar lr0Automaton productions

    failwith "TODO"