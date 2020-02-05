namespace Kedr.ParserGenerator.LR0

open Kedr.ParserGenerator

type internal Automaton<'symbol when 'symbol : comparison> =
    private {
        _transitions : StateTransition<'symbol> Set
        _states : State<'symbol> Set
    }
    member this.transitions = this._transitions
    member this.states = this._states

module internal Automaton =
    let private createTransitionsOneLevel
        (productions : Production<_> Set)
        (state : State<_>)
        : StateTransition<_> Set =
        seq {
            let nonFinalConfigsByAheadSymbol =
                state.configurations
                |> Seq.groupBy Configuration.tryAheadSymbol
                |> Seq.choose (fun (smbl, cfgs) -> option { let! smbl = smbl in return (smbl, cfgs) })

            for (aheadSymbol, configs) in nonFinalConfigsByAheadSymbol ->
                let nextAutomatonState =
                    [
                        for config in configs do
                            let basisConfig = { config with cursorOffset = config.cursorOffset + 1 }
                            yield basisConfig
                            yield! basisConfig |> Configuration.close productions
                    ]
                    |> Set.ofList
                    |> fun configs -> { State.configurations = configs }

                { sourceState = state
                  symbol = aheadSymbol
                  destinationState = nextAutomatonState }
        }
        |> Set.ofSeq

    let private createTransitions
        (productions : Production<_> Set)
        (initialState : State<_>)
        : StateTransition<_> Set =
        let rec loopCreateTransitions newStates allStates transitions =
            let transitionsOfNewStates =
                newStates
                |> Seq.collect (createTransitionsOneLevel productions)
                |> Set.ofSeq

            let transitions = transitions + transitionsOfNewStates

            let producedStates = transitionsOfNewStates |> Set.map (fun t -> t.destinationState)
            let newStates = producedStates - allStates

            if not newStates.IsEmpty then
                let allStates = newStates + allStates
                loopCreateTransitions newStates allStates transitions
            else
                transitions

        let states = [ initialState ] |> Set.ofList

        loopCreateTransitions states states Set.empty

    let create (grammar : Grammar<_>) =
        let initialState = // closed start configurations of S
            grammar.productions
            |> Seq.filter (fun prod -> grammar.startingSymbols |> Set.contains prod.from)
            |> Seq.collect (fun prod -> seq {
                let state = Configuration.createStart prod
                yield state
                yield! Configuration.close grammar.productions state
                })
            |> Set.ofSeq
            |> fun configs -> { State.configurations = configs }

        let transitions = createTransitions grammar.productions initialState

        let states =
            seq {
                for tr in transitions do
                    yield tr.sourceState
                    yield tr.destinationState
            } |> Set.ofSeq

        { _transitions = transitions
          _states = states }
