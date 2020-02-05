module Kedr.ParserGenerator.LALRAutomaton

open Kedr.ParserGenerator

type private AugmentedSymbol<'s when 's : comparison> =
    | PlainSymbol of 's
    | TransitionalSymbol of 's * State<'s> * State<'s>

let private createAugmentedGrammar (lr0 : LR0Automaton<_>) (grammar : Grammar<_>) : Grammar<_> =
    let tryTransition state symbol : State<_> option =
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

    let rec traceAndAugmentSymbols state remainingSymbols reverseResult =
        match remainingSymbols with
        | [] -> reverseResult |> List.rev
        | symbol :: symbolsRest ->
            let state' = transition state symbol
            let symbol' =
                if grammar.nonTerminals |> Set.contains symbol
                then TransitionalSymbol (symbol, state, state')
                else PlainSymbol symbol

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
                        | None -> PlainSymbol A
                        | Some trState -> TransitionalSymbol (A, state, trState)

                    let w' = traceAndAugmentSymbols state w []

                    yield {
                        from = A'
                        into = w'
                    }
        } |> Set.ofSeq

    Grammar.fromProductions productions'

type private FollowingSymbol<'s> =
    | EOF
    | Symbol of 's
    override this.ToString () =
        match this with
        | EOF -> "$"
        | Symbol s -> s.ToString()

let private createFollowSets (grammar : Grammar<'s>) : Map<'s, FollowingSymbol<'s> Set> =
    let firstSets =
        grammar.symbols
        |> Seq.map (fun s -> (s, DependentSet<'s>()))
        |> Map.ofSeq

    for symbol in grammar.terminals do
        firstSets.[symbol].Add(symbol)

    for production in grammar.productions do
        let firstOfInto = production.into |> List.head
        firstSets.[production.from].Add(firstSets.[firstOfInto])

    let firstSets = firstSets |> Map.map (fun _ set -> set.ToSet())

    let followSets =
        grammar.symbols
        |> Seq.map (fun s -> (s, DependentSet()))
        |> Map.ofSeq

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

    let followSets' = followSets |> Map.map (fun _ set -> set.ToSet())

    failwith "TODO"

let create (grammar : Grammar<_>) =
    let lr0 = LR0Automaton.create grammar

    let augmentedGrammar = createAugmentedGrammar lr0 grammar

    let followSet = createFollowSets augmentedGrammar

    failwith "TODO"