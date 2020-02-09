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

type internal GotoKey<'a when 'a : comparison> = {
    state : State<'a>
    observedSymbol : 'a
}

type internal ActionKey<'a when 'a : comparison> = {
    state : State<'a>
    lookaheadSymbol : 'a
}

type internal ParsingTable<'a when 'a : comparison> = {
    goto : DefaultingMap<GotoKey<'a>, State<'a>>
    action : DefaultingMap<ActionKey<'a>, Action<'a>>
}

module internal ParsingTable =
    let create (automaton : Automaton<_>) =
        let action =
            seq {
                for state in automaton.states do
                    let finalConfigurations =
                        state.configurations |> Set.filter (fun cfg -> cfg.production.into.Length = cfg.cursorOffset)

                    for cfg in finalConfigurations do
                        for lookahead in cfg.lookahead do
                            yield ({ state = state; lookaheadSymbol = lookahead }, Reduce cfg.production)
            }
            |> DefaultingMap.ofSeq Shift

        let goto =
            failwith ""

        { action = action; goto = goto }
