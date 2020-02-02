module Kedr.ParserGenerator.LALRGenerator

type private AugmentedSymbol =
    | StartingSymbol of Symbol
    | TransitionSymbol of Symbol * State * State

let private createAugmentedGrammar
    (lr0Automaton : LR0Automaton)
    (productions : Production Set)
    : Production Set =

    let nonTerminals =
        productions
        |> Seq.map (fun p -> p.from)
        |> Set.ofSeq

    let augmentedNonTerminals =
        seq {
            for nonTerminal in nonTerminals do
                let transitionalSymbols =
                    lr0Automaton.transitions
                    |> Seq.choose (fun tr ->
                        if nonTerminal = tr.symbol then
                            TransitionSymbol (nonTerminal, tr.sourceState, tr.destinationState)
                            |> Some
                        else
                            None)
                    |> Set.ofSeq
                if not (transitionalSymbols |> Set.isEmpty) then
                    yield! transitionalSymbols
                else
                    yield StartingSymbol nonTerminal
        }
        |> Set.ofSeq
//        |> Set.map (fun symbol ->
//            match symbol with
//            | StartingSymbol symbol -> (symbol, symbol)
//            | TransitionSymbol (Symbol(symbolText), stateFrom, stateInto) ->
//                let newSymbol = Symbol(sprintf "%s_%s_%s" symbolText stateFrom))

    failwith "TODO"

let generate (productions : Production Set) =

    let lr0Automaton = LR0Automaton.generate productions

    let augmentedProductions = createAugmentedGrammar lr0Automaton productions

    failwith "TODO"