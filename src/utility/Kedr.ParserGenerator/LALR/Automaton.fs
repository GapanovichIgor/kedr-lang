namespace Kedr.ParserGenerator.LALR

open System.Collections.Generic
open Kedr.ParserGenerator

type internal Automaton<'symbol when 'symbol : comparison> =
    private {
        _grammar : Grammar<'symbol>
        _transitions : StateTransition<'symbol> Set
        _states : State<'symbol> Set
    }
    member this.grammar = this._grammar
    member this.transitions = this._transitions
    member this.states = this._states

module internal Automaton =
    type private AugmentedSymbol<'s when 's : comparison> =
        | PlainSymbol of 's
        | TransitionalSymbol of 's * LR0.State<'s> * LR0.State<'s>

    type private LookaheadKey<'s when 's : comparison> = LR0.State<'s> * LR0.Configuration<'s>

    let private createAugmentedGrammar
            (lr0 : LR0.Automaton<'s>)
            (grammar : Grammar<'s>)
            : Grammar<AugmentedSymbol<'s>> =

        let tryTransition state symbol : LR0.State<'s> option =
            let transitionsFromStateOnSymbol =
                lr0.transitions
                |> Seq.filter (fun tr -> tr.sourceState = state && tr.symbol = symbol)
                |> List.ofSeq

            match transitionsFromStateOnSymbol with
            | [] -> None
            | [tr] -> Some tr.destinationState
            | _ -> failwith "LR0 contains reduce/reduce conflict"

        let rec traceAndAugmentSymbols state inputSymbols reverseResult : AugmentedSymbol<'s> list =
            match inputSymbols with
            | [] -> reverseResult |> List.rev
            | symbol :: inputSymbolsRest ->
                let state' = tryTransition state symbol |> Option.get
                let symbol' =
                    if grammar.nonTerminals |> Set.contains symbol
                    then TransitionalSymbol (symbol, state, state')
                    else PlainSymbol symbol

                let reverseResult = symbol' :: reverseResult

                traceAndAugmentSymbols state' inputSymbolsRest reverseResult

        let augmentedProductions : Set<Production<AugmentedSymbol<'s>>> =
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

        Grammar.fromProductions augmentedProductions

    let private createFollowSets
            (eof : 's)
            (grammar : Grammar<AugmentedSymbol<'s>>)
            : Map<AugmentedSymbol<'s>, 's Set> =

        let createEmptyMap () =
            grammar.symbols
            |> Seq.map (fun s -> (s, DependentSet<'s>()))
            |> Map.ofSeq

        let firstSets = createEmptyMap ()

        for symbol in grammar.terminals do
            match symbol with
            | PlainSymbol plain -> firstSets.[symbol].Add(plain)
            | TransitionalSymbol _ -> failwith "Terminal can not be a TransitionalSymbol"

        for production in grammar.productions do
            let firstOfInto = production.into |> List.head
            firstSets.[production.from].Add(firstSets.[firstOfInto])

        let firstSets = firstSets |> Map.map (fun _ set -> set.ToSet())

        let followSets = createEmptyMap ()

        for symbol in grammar.startingSymbols do
            followSets.[symbol].Add(eof)

        for production in grammar.productions do
            let lastOfInto = production.into |> List.last
            followSets.[lastOfInto].Add(followSets.[production.from])

            production.into
            |> Seq.pairwise
            |> Seq.iter (fun (a, b) ->
                firstSets.[b]
                |> Seq.iter followSets.[a].Add)

        followSets |> Map.map (fun _ set -> set.ToSet())

    let private createLookahead
            (lr0 : LR0.Automaton<'s>)
            (followSets : Map<AugmentedSymbol<'s>, Set<'s>>)
            : Map<LookaheadKey<'s>, Set<'s>> =

        let rec trace startingState inputSymbols : LR0.State<'s> =
            match inputSymbols with
            | [] -> startingState
            | symbol :: inputSymbolsRest ->
                let transitionFromStateOnSymbol =
                    lr0.transitions
                    |> Seq.find (fun tr -> tr.symbol = symbol && tr.sourceState = startingState)

                let state' = transitionFromStateOnSymbol.destinationState

                trace state' inputSymbolsRest

        let mutable lookaheadMap = Map.empty
        for state in lr0.states do
            let startConfigs = state.configurations |> Set.filter (fun c -> c.cursorOffset = 0)
            for config in startConfigs do
                // config: A -> . w
                let A = config.production.from
                let transitionStateOnA =
                    lr0.transitions
                    |> Seq.tryFind (fun tr -> tr.symbol = A && tr.sourceState = state)
                    |> Option.map (fun tr -> tr.destinationState)
                let lookahead =
                    match transitionStateOnA with
                    | Some trState -> followSets.[TransitionalSymbol(A, state, trState)]
                    | None -> followSets.[PlainSymbol A]

                let endState = trace state config.production.into

                let endConfig = { config with cursorOffset = config.production.into.Length }

                let key = (endState, endConfig)

                lookaheadMap <-
                    match lookaheadMap |> Map.tryFind key with
                    | Some existingLookahead -> lookaheadMap |> Map.add key (existingLookahead + lookahead)
                    | None -> lookaheadMap |> Map.add key lookahead

        lookaheadMap

    let private createLalrTransitions
            (lr0 : LR0.Automaton<'s>)
            (lookaheads : Map<LookaheadKey<'s>, Set<'s>>)
            : Set<StateTransition<'s>> =

        let toLalrState (lr0State : LR0.State<'s>) : State<'s> =
            let configurations =
                lr0State.configurations
                |> Set.map (fun cfg ->
                    let lookahead =
                        match lookaheads.TryGetValue((lr0State, cfg)) with
                        | (true, la) -> la
                        | _ -> Set.empty
                    {
                        production = cfg.production
                        cursorOffset = cfg.cursorOffset
                        lookahead = lookahead
                    })

            { configurations = configurations }

        let lalrTransitions =
            lr0.transitions
            |> Set.map (fun tr ->
                { sourceState = toLalrState tr.sourceState
                  symbol = tr.symbol
                  destinationState = toLalrState tr.destinationState })

        lalrTransitions

    let create (eof : 's) (grammar : Grammar<'s>) : Automaton<'s> =
        let lr0 = LR0.Automaton.create grammar

        let augmentedGrammar = createAugmentedGrammar lr0 grammar

        let followSets = createFollowSets eof augmentedGrammar

        let lookaheads = createLookahead lr0 followSets

        let lalrTransitions = createLalrTransitions lr0 lookaheads

        let states =
            seq {
                for tr in lalrTransitions do
                    yield tr.sourceState
                    yield tr.destinationState
            } |> Set.ofSeq

        { _transitions = lalrTransitions
          _states = states
          _grammar = grammar }