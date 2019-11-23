module Kedr.ParserGenerator.LALRGenerator

type AutomatonState = AutomatonState of Set<ProductionState>
type Automaton = Map<(AutomatonState * Symbol), AutomatonState>


let private createInitialState (production : Production) : ProductionState =
    { production = production; cursorOffset = 0 }

let rec private getEpsilonTransition basisProductionState productions = [
    // if basis state is A -> α.Bω where B is non-terminal then get ε transition to B
    // i. e. include all productions of B at initial state (B -> .γ)
    if basisProductionState.production.into.Length > basisProductionState.cursorOffset then
        let nextSymbol = basisProductionState.production.into.[basisProductionState.cursorOffset]
        let nextSymbolProductions = productions |> Set.filter (fun prod -> prod.from = nextSymbol)
        let otherProductions = productions - nextSymbolProductions
        for prod in nextSymbolProductions do
            let state = createInitialState prod
            yield state
            yield! getEpsilonTransition state otherProductions
    ]

let private createLr0Goto
    (initialState : AutomatonState)
    (productions : Production Set)
    (createState : AutomatonState -> AutomatonState) =

    let getTransitions (AutomatonState productionStates) : (Symbol * AutomatonState) Set =
        [
            let incompleteProdStatesBySymbol =
                productionStates
                |> Seq.filter (fun prodState -> prodState.production.into.Length > prodState.cursorOffset)
                |> Seq.groupBy (fun prodState -> prodState.production.into.[prodState.cursorOffset])
            for (symbol, prodStates) in incompleteProdStatesBySymbol do
                let nextAutomatonState =
                    [
                        for prodState in prodStates do
                            let basisProdState = { prodState with cursorOffset = prodState.cursorOffset + 1 }
                            yield basisProdState
                            yield! getEpsilonTransition basisProdState productions
                    ]
                    |> Set.ofList
                    |> AutomatonState
                    |> createState
                yield (symbol, nextAutomatonState)
        ]
        |> Set.ofList

    let rec createGoto (newStates : AutomatonState Set) allStates map =
        let mutable map = map
        for state in newStates do
            for (symbol, nextState) in getTransitions state do
                map <- map |> Map.add (state, symbol) nextState

        let statesInMap =
            map
            |> Map.toSeq
            |> Seq.map snd
            |> Set

        let newStates = statesInMap - allStates

        if not newStates.IsEmpty then
            let allStates = newStates + allStates
            createGoto newStates allStates map
        else
            map

    let states = [ initialState ] |> Set.ofList

    createGoto states states Map.empty

let private createLr0Automaton (startingSymbol : Symbol) (productions : Production Set) : Automaton =
    let initialState = // all productions of S at initial state + ε transitions
        productions
        |> Seq.filter (fun prod -> prod.from = startingSymbol)
        |> Seq.collect (fun prod -> seq {
            let state = createInitialState prod
            yield state
            yield! getEpsilonTransition state productions
            })
        |> Set.ofSeq
        |> AutomatonState

    let mutable automatonStateSet = Map.ofList [initialState, initialState]

    let createState (state : AutomatonState) =
        match automatonStateSet |> Map.tryFind state with
        | Some state -> state
        | None ->
            automatonStateSet <- automatonStateSet |> Map.add state state
            state

    let result = createLr0Goto initialState productions createState

    result

let generate (productions : Production Set) =
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

    printf "%A" lr0

    failwith "TODO"