namespace Kedr.ParserGenerator
open Kedr.ParserGenerator.LALR

type internal Action<'a> =
    | Shift
    | Reduce of Production<'a>
    | Accept

type internal ParsingTable<'s when 's : comparison> = private {
    grammar : Grammar<'s>
    goto : Map<State<'s>, Map<'s, State<'s>>>
    action : Map<State<'s>, Map<'s, Action<'s>>>
}

module internal ParsingTable =
    let create (automaton : Automaton<_>) =
        let action =
            seq {
                for state in automaton.states do
                    let finalConfigurations = state.configurations |> Set.filter Configuration.isFinal

                    let stateActions =
                        seq {
                            for cfg in finalConfigurations do
                                for lookahead in cfg.lookahead do
                                    let action =
                                        if cfg.production.from = automaton.grammar.startingSymbol
                                        then Accept
                                        else Reduce cfg.production

                                    yield (lookahead, action)

                            let symbolsWithTransitionFromState =
                                automaton.transitions
                                |> Seq.filter (fun tr -> tr.sourceState = state)
                                |> Seq.map (fun tr -> tr.symbol)
                                |> Seq.filter (automaton.grammar.terminals.Contains)

                            for symbol in symbolsWithTransitionFromState do
                                yield (symbol, Shift)
                        } |> Map.ofSeq

                    yield (state, stateActions)
            } |> Map.ofSeq

        let goto =
            seq {
                for state in automaton.states do
                    let nonFinalConfigurations = state.configurations |> Set.filter (not << Configuration.isFinal)

                    let stateGoto =
                        seq {
                            for cfg in nonFinalConfigurations do
                                match Configuration.getSymbolAfterCursor cfg with
                                | None -> ()
                                | Some symbol ->
                                    if automaton.grammar.nonTerminals.Contains(symbol) then
                                        let transitionOnSymbol =
                                            automaton.transitions
                                            |> Seq.filter (fun tr -> tr.sourceState = state && tr.symbol = symbol)
                                            |> Seq.exactlyOne

                                        yield (symbol, transitionOnSymbol.destinationState)
                        } |> Map.ofSeq

                    yield (state, stateGoto)
            } |> Map.ofSeq

        { grammar = automaton.grammar
          action = action
          goto = goto }
