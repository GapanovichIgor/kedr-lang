module Kedr.AST
open System

type private Symbol = Symbol of Guid

type private Production = {
    from : Symbol
    into : Symbol list
    }

let private symbol () = Symbol (Guid.NewGuid())

let private ident = symbol()
let private quot_str = symbol()
let private num = symbol()

let private VAL_ATOM = symbol()
let private PREFIX_APP = symbol()

let private (=>) from into = { from = from; into = into }

let private productions = [
    PREFIX_APP => [ VAL_ATOM ]
    PREFIX_APP => [ PREFIX_APP; VAL_ATOM ]
    VAL_ATOM => [ quot_str ]
    VAL_ATOM => [ num ]
    VAL_ATOM => [ ident ]
    ]

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

let private lr0Automaton productions =
    let initialState = [ for prod in productions -> { production = prod; cursorOffset = 0 } ]

    let goto = createLr0Goto initialState

    ()

let test = lr0Automaton productions