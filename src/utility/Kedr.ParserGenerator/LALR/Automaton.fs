namespace Kedr.ParserGenerator.LALR

open System.Collections.Generic
open Kedr.ParserGenerator

type internal Automaton<'symbol when 'symbol : comparison> =
    private {
        _transitions : StateTransition<'symbol> Set
        _states : State<'symbol> Set
    }
    member this.transitions = this._transitions
    member this.states = this._states

module internal Automaton =
    type private AugmentedSymbol<'s when 's : comparison> =
        | PlainSymbol of 's
        | TransitionalSymbol of 's * LR0.State<'s> * LR0.State<'s>

    let private createAugmentedGrammar (lr0 : LR0.Automaton<'s>) (grammar : Grammar<'s>) : Grammar<AugmentedSymbol<'s>> =
        let tryTransition state symbol : LR0.State<_> option =
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

    let private createFollowSets (eof : 's) (grammar : Grammar<AugmentedSymbol<'s>>) : Map<AugmentedSymbol<'s>, 's Set> =
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

    let private addLookahead (lr0 : LR0.Automaton<_>) (followSets : Map<_,_>) : Automaton<_> =

        let rec traceToState state symbols =
            match symbols with
            | [] -> state
            | s :: sRest ->
                let state' =
                    lr0.transitions
                    |> Seq.find (fun tr -> tr.symbol = s && tr.sourceState = state)
                    |> fun tr -> tr.destinationState
                traceToState state' sRest

        let lookaheadsByStateAndCfg = Dictionary()

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

                let endState = traceToState state config.production.into

                let endConfig = { config with cursorOffset = config.production.into.Length }

                lookaheadsByStateAndCfg.[(endState, endConfig)] <- lookahead

        let toLalrState (lr0State : LR0.State<_>) = {
            configurations =
                lr0State.configurations
                |> Set.map (fun cfg ->
                    let lookahead = lookaheadsByStateAndCfg.[(lr0State, cfg)]
                    {
                        production = cfg.production
                        cursorOffset = cfg.cursorOffset
                        lookahead = lookahead
                    })
            }

        let transitions =
            seq {
                for tr in lr0.transitions do
                    {
                        sourceState = toLalrState tr.sourceState
                        symbol = tr.symbol
                        destinationState = toLalrState tr.destinationState
                    }
            } |> Set.ofSeq

        let states =
            seq {
                for tr in transitions do
                    yield tr.sourceState
                    yield tr.destinationState
            } |> Set.ofSeq

        { _transitions = transitions
          _states = states }

    let create (eof : 's) (grammar : Grammar<'s>) : Automaton<'s> =
        let lr0 = LR0.Automaton.create grammar

        let augmentedGrammar = createAugmentedGrammar lr0 grammar

        let followSets = createFollowSets eof augmentedGrammar

        addLookahead lr0 followSets