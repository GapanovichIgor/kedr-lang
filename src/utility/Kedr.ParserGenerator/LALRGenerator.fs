module Kedr.ParserGenerator.LALRGenerator

type private ProductionState = {
    production : Production
    cursorOffset : int 
    }

type private AutomatonState = ProductionState list

let private createLr0Goto initialState =
    let getTransitions (state : AutomatonState) : (Symbol * AutomatonState) list = [
        let incompleteProdStatesBySymbol =
            state
            |> Seq.filter (fun prodState -> prodState.production.into.Length > prodState.cursorOffset)
            |> Seq.groupBy (fun prodState -> prodState.production.into.[prodState.cursorOffset])

        for (symbol, prodStates) in incompleteProdStatesBySymbol do
            let nextState = [
                for prodState in prodStates ->
                    { prodState with cursorOffset = prodState.cursorOffset + 1 }
                ]
            yield (symbol, nextState)
        ]

    let rec createGoto newStates allStates map =
        let mutable map = map
        for state in newStates do
            for (symbol, nextState) in getTransitions state do
                map <- map |> Map.add (state, symbol) nextState

        let newStates =
            let mapValues = map |> Map.toSeq |> Seq.map snd
            mapValues
            |> Seq.except allStates
            |> Seq.toList

        if not newStates.IsEmpty then
            let allStates = newStates @ allStates
            createGoto newStates allStates map
        else
            map

    createGoto [ initialState ] [ initialState ] Map.empty

let private createLr0Automaton startingSymbol productions =
    let initialState =
        productions
        |> Seq.filter (fun prod -> prod.from = startingSymbol)
        |> Seq.map (fun prod -> { production = prod; cursorOffset = 0 })
        |> List.ofSeq

    let goto = createLr0Goto initialState

    ()

let generate (productions : Production list) =
    let nonTerminals =
        productions
        |> Seq.map (fun prod -> prod.from)
        |> Seq.distinct
        |> List.ofSeq

    let producedSymbols =
        productions
        |> Seq.collect (fun prod -> prod.into)
        |> Seq.distinct
        |> Seq.toList

    let startingSymbol =
        nonTerminals
        |> Seq.except producedSymbols
        |> Seq.exactlyOne

    let lr0 = createLr0Automaton startingSymbol productions

    failwith "TODO"