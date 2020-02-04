module Kedr.ParserGenerator.LALRAutomaton

open Kedr.ParserGenerator

let private createAugmentedGrammar (lr0 : LR0Automaton) (grammar : Grammar) : Grammar =
    let stateNumbers =
        lr0.states
        |> Seq.mapi (fun i state -> (state, i))
        |> Map.ofSeq

    let tryTransition state symbol : State option =
        let transitions =
            lr0.transitions
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
                if grammar.nonTerminals |> Set.contains symbol
                then createAugmentedSymbol symbol state state'
                else symbol

            let reverseResult = symbol' :: reverseResult

            traceAndAugmentSymbols state' symbolsRest reverseResult

    let productions' =
        seq {
            for state in lr0.states do
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

    Grammar.fromProductions productions'

type private FollowingSymbol =
    | EOF
    | Symbol of Symbol

let private createFollowSets (grammar : Grammar) : Map<Symbol, FollowingSymbol Set> =
    let createEmptySetMap () =
        grammar.symbols
        |> Seq.map (fun s -> (s, DependentSet()))
        |> Map.ofSeq

    let firstSets = createEmptySetMap ()

    for symbol in grammar.terminals do
        firstSets.[symbol].Add(symbol)

    for production in grammar.productions do
        let firstOfInto = production.into |> List.head
        firstSets.[production.from].Add(firstSets.[firstOfInto])

    let firstSets = firstSets |> Map.map (fun _ set -> set.ToSet())

    let followSets = createEmptySetMap ()

    for symbol in grammar.startingSymbols do
        followSets.[symbol].Add(EOF)

    for production in grammar.productions do
        let lastOfInto = production.into |> List.last
        followSets.[lastOfInto].Add(followSets.[production.from])

        production.into
        |> Seq.pairwise
        |> Seq.iter (fun (a, b) ->
            firstSets.[b]
            |> Seq.map (fun s -> Symbol s)
            |> Seq.iter followSets.[a].Add)

    let followSets = followSets |> Map.map (fun _ set -> set.ToSet())

    failwith "TODO"

let create (grammar : Grammar) =
    let lr0 = LR0Automaton.create grammar

    let augmentedGrammar = createAugmentedGrammar lr0 grammar

    let followSet = createFollowSets augmentedGrammar

    failwith "TODO"