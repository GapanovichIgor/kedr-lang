namespace Kedr.ParserGenerator
open Kedr.ParserGenerator.LALR

type internal DefaultingMap<'k , 'v when 'k : comparison> =
    private {
        map : Map<'k, 'v>
        defaultVal : 'v
    }

module internal DefaultingMap =
    let empty defVal = { map = Map.empty; defaultVal = defVal }

    let add k v dmap = { dmap with map = dmap.map |> Map.add k v }

    let find k dmap =
        match dmap.map |> Map.tryFind k with
        | Some v -> v
        | None -> dmap.defaultVal

    let ofSeq defVal kvs =
        let map = kvs |> Map.ofSeq
        { map = map; defaultVal = defVal }

type internal Action<'a> =
    | Shift
    | Reduce of Production<'a>
    | Accept
    | Reject

type internal TableKey<'a when 'a : comparison> = {
    state : State<'a>
    symbol : 'a
}

type internal ParsingTable<'a when 'a : comparison> = {
    goto : Map<TableKey<'a>, State<'a>>
    action : DefaultingMap<TableKey<'a>, Action<'a>>
}

module internal ParsingTable =
    let create (automaton : Automaton<_>) =
        let action : DefaultingMap<TableKey<'s>, Action<'s>> =
            seq {
                for state in automaton.states do
                    let finalConfigurations = state.configurations |> Set.filter Configuration.isFinal

                    for cfg in finalConfigurations do
                        for lookahead in cfg.lookahead do
                            let action =
                                if cfg.production.from = automaton.grammar.startingSymbol
                                then Accept
                                else Reduce cfg.production

                            yield ({ state = state; symbol = lookahead }, action)

                    let symbolsWithTransitionFromState =
                        automaton.transitions
                        |> Seq.filter (fun tr -> tr.sourceState = state)
                        |> Seq.map (fun tr -> tr.symbol)
                        |> Seq.filter (automaton.grammar.terminals.Contains)

                    for symbol in symbolsWithTransitionFromState do
                        yield ({ state = state; symbol = symbol }, Shift)
            } |> DefaultingMap.ofSeq Reject

        let goto =
            seq {
                for state in automaton.states do
                    let nonFinalConfigurations = state.configurations |> Set.filter (not << Configuration.isFinal)

                    for cfg in nonFinalConfigurations do
                        match Configuration.getSymbolAfterCursor cfg with
                        | None -> ()
                        | Some symbol ->
                            if automaton.grammar.nonTerminals.Contains(symbol) then
                                let transitionOnSymbol =
                                    automaton.transitions
                                    |> Seq.filter (fun tr -> tr.sourceState = state && tr.symbol = symbol)
                                    |> Seq.exactlyOne

                                yield ({ state = state; symbol = symbol }, transitionOnSymbol.destinationState)
            } |> Map.ofSeq

        { action = action; goto = goto }
