module Kedr.ParserGenerator.LALRGenerator

type AutomatonState = AutomatonState of Configuration Set

type Automaton = Map<(AutomatonState * Symbol), AutomatonState>

let private createStartConfiguration (production : Production) : Configuration =
    { production = production; cursorOffset = 0 }

let rec private closeConfiguration (configuration : Configuration) (productions : Production Set) : Configuration list = [
    // if configuration is A -> α.Bω where B is non-terminal then
    // get all productions of B at start configuration (B -> .γ)
    if configuration.production.into.Length > configuration.cursorOffset then
        let symbolToExpand = configuration.production.into.[configuration.cursorOffset]
        let symbolToExpandProductions = productions |> Set.filter (fun prod -> prod.from = symbolToExpand)
        let unusedProductions = productions - symbolToExpandProductions
        for prod in symbolToExpandProductions do
            let startConfiguration = createStartConfiguration prod
            yield startConfiguration
            yield! closeConfiguration startConfiguration unusedProductions
    ]

let private createLr0Goto
    (initialState : AutomatonState)
    (productions : Production Set)
    (automatonStates : AutomatonState Bag)
    : Map<(AutomatonState * Symbol), AutomatonState> =

    let mutable automatonStates = automatonStates
    let findExistingOrEnlistState state =
        let (state, bag) = automatonStates |> Bag.getExistingOrAdd state
        automatonStates <- bag
        state

    let getTransitions (automatonState : AutomatonState) : (Symbol * AutomatonState) Set =
        [
            let (AutomatonState productionStates) = automatonState
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
                            yield! closeConfiguration basisProdState productions
                    ]
                    |> Set.ofList
                    |> AutomatonState
                    |> findExistingOrEnlistState
                yield (symbol, nextAutomatonState)
        ]
        |> Set.ofList

    let rec createGoto newStates allStates map =
        let mutable map = map
        for state in newStates do
            for (symbol, nextState) in getTransitions state do
                map <- map |> Map.add (state, symbol) nextState

        let statesInMap =
            map
            |> Map.toSeq
            |> Seq.map snd
            |> Set.ofSeq

        let newStates = statesInMap - allStates

        if not newStates.IsEmpty then
            let allStates = newStates + allStates
            createGoto newStates allStates map
        else
            map

    let states = [ initialState ] |> Set.ofList

    createGoto states states Map.empty

let private createLr0Automaton (startingSymbol : Symbol) (productions : Production Set) : Automaton =
    let initialState = // closed start configurations of S
        productions
        |> Seq.filter (fun prod -> prod.from = startingSymbol)
        |> Seq.collect (fun prod -> seq {
            let state = createStartConfiguration prod
            yield state
            yield! closeConfiguration state productions
            })
        |> Set.ofSeq
        |> AutomatonState

    let automatonStates = Bag.singleton initialState

    let result = createLr0Goto initialState productions automatonStates

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