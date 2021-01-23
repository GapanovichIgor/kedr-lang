module internal Kedr.ParserGenerator.CodeGen.CodeGenerator
#nowarn "0058"

open Kedr.ParserGenerator
open System.IO

let private writeComment (writer : StreamWriter) =
    let put = fprintfn writer
    put "//-----------------------------------------------------------------------"
    put "// This code was generated by a tool."
    put "// Changes to this file may cause incorrect behavior and will be lost if"
    put "// the code is regenerated."
    put "//-----------------------------------------------------------------------"

let private symbolToTypeVariable s =
    sprintf "'%s" (s.ToString())

let private typeVariableListOf symbols =
    symbols
    |> Seq.map symbolToTypeVariable
    |> Seq.sortDescending
    |> String.concat ", "

let private typeVariableMapOf symbols =
    symbols
    |> Seq.map (fun s -> (s, symbolToTypeVariable s))
    |> Map.ofSeq

let private writeTerminalType (grammar : Grammar<_>) writer =
    let put fmt = fprintfn writer fmt

    let typeVariableList = typeVariableListOf grammar.terminals

    let orderedTerminals =
        grammar.terminals
        |> Seq.sortBy StableSorting.keyOfSymbol
        |> Seq.map (fun t -> (t.ToString(), symbolToTypeVariable t))
        |> List.ofSeq

    put "type Terminal<%s> =" typeVariableList
    for (terminalStr, typeVariable) in orderedTerminals do begin
    put "    | T_%s of %s" terminalStr typeVariable
    end

let private writeReducerType (grammar : Grammar<_>) writer =
    let put fmt = fprintfn writer fmt

    let typeVariableMap = typeVariableMapOf grammar.symbols
    let typeVariableList = typeVariableListOf grammar.symbols

    fprintfn writer "type Reducer<%s> = {" typeVariableList

    seq {
        for production in grammar.productions do
            let fieldName =
                let fromString = production.from.ToString()
                let intoString = production.into |> Seq.map (fun s -> s.ToString()) |> String.concat "_"
                sprintf "%s_%s" fromString intoString

            let parameter =
                production.into
                |> Seq.map (fun s -> typeVariableMap.[s])
                |> String.concat " * "

            let result = typeVariableMap.[production.from]

            sprintf "    %s : (%s) -> %s" fieldName parameter result
    }
    |> Seq.sort
    |> Seq.iter writer.WriteLine

    fprintfn writer "}"

let private writeParseFunction (grammar : Grammar<_>) (automaton : LALR.Automaton<_>) writer =
    let symbolTypeVar =
        let occupiedNames = grammar.symbols |> Set.map symbolToTypeVariable
        let rec go name =
            if occupiedNames.Contains(name)
            then go (name + "_")
            else name
        go "'symbol"

    let startingSymbol =
        grammar.startingSymbols
        |> Seq.tryExactlyOne
        |> Option.defaultWith (fun () -> failwith "mutliple starting symbols are not supported")

    let orderedStates =
        automaton.states
        |> Seq.sortBy StableSorting.keyOfState
        |> List.ofSeq

    let findStateWhereConfigurationExists fn =
        automaton.states
        |> Seq.filter (fun st ->
            st.configurations
            |> Seq.exists fn)
        |> Seq.exactlyOne

    let startingState =
        findStateWhereConfigurationExists (fun cfg ->
            cfg.production.from = startingSymbol &&
            cfg.cursorOffset = 0)

    let acceptingState =
        findStateWhereConfigurationExists (fun cfg ->
            cfg.production.from = startingSymbol &&
            cfg.cursorOffset = cfg.production.into.Length)

    let orderedTerminals =
        grammar.terminals
        |> Seq.sortBy StableSorting.keyOfSymbol
        |> List.ofSeq

    let transitionExists state symbol =
        automaton.transitions
        |> Seq.exists (fun tr ->
            tr.sourceState = state &&
            tr.symbol = symbol)

    fprintfn writer "let parse<%s, %s>" symbolTypeVar (typeVariableListOf grammar.symbols)
    fprintfn writer "    (recognizeTerminal : %s -> Terminal<%s>)" symbolTypeVar (typeVariableListOf grammar.terminals)
    fprintfn writer "    (reducer : Reducer<%s>)" (typeVariableListOf grammar.symbols)
    fprintfn writer "    (symbols : seq<%s>)" symbolTypeVar

    fprintfn writer "    : Result<%s, string> =" (symbolToTypeVariable startingSymbol)
    fprintfn writer ""

    fprintfn writer "    use enumerator = symbols.GetEnumerator()"
    fprintfn writer "    let mutable state = %i" (orderedStates |> List.findIndex ((=) startingState))
    fprintfn writer "    let mutable observedSymbol = Unchecked.defaultof<'symbol>"
    fprintfn writer "    let mutable observedSymbolIsEof = false"
    fprintfn writer "    let mutable lookaheadSymbol = Unchecked.defaultof<'symbol>"
    fprintfn writer "    let mutable lookaheadSymbolIsEof = false"
    fprintfn writer "    let symbolStack = System.Collections.Stack()"
    fprintfn writer "    let stateStack = System.Collections.Stack()"
    fprintfn writer "    let mutable keepGoing = true"
    fprintfn writer "    let mutable success = false"
    fprintfn writer ""
    fprintfn writer "    let shift () ="
    fprintfn writer "        observedSymbol <- lookaheadSymbol"
    fprintfn writer "        observedSymbolIsEof <- lookaheadSymbolIsEof"
    fprintfn writer "        if not lookaheadSymbolIsEof then"
    fprintfn writer "            if enumerator.MoveNext()"
    fprintfn writer "            then lookaheadSymbol <- enumerator.Current"
    fprintfn writer "            else lookaheadSymbolIsEof <- true"
    fprintfn writer ""
    fprintfn writer "    let accept () ="
    fprintfn writer "        keepGoing <- false"
    fprintfn writer "        success <- true"
    fprintfn writer ""
    fprintfn writer "    shift ()"
    fprintfn writer "    shift ()"
    fprintfn writer ""
    fprintfn writer "    while keepGoing do"
    fprintfn writer "        match state with"
    for (i, state) in orderedStates |> Seq.indexed do
        fprintfn writer "        | %i ->" i


        fprintf writer "            if observedSymbolIsEof then "
        if state = acceptingState
        then fprintfn writer "accept () else"
        else fprintfn writer "failwith \"TODO reject\" else"

        fprintfn writer "            match recognizeTerminal observedSymbol with"
        for terminal in orderedTerminals do
            let terminalName = sprintf "T_%s" (terminal.ToString())
            if transitionExists state terminal then
                fprintfn writer "            | %s t -> ()" terminalName
            else
                fprintfn writer "            | %s _ -> failwith \"TODO reject\"" terminalName

        fprintfn writer "            ()"
    fprintfn writer "        | _ -> failwith \"Parser is in an invalid state. This is a bug in the parser generator.\""

    fprintfn writer "    failwith \"TODO\""



let generate
    (eof : string)
    (definition : ParserDefinition)
    (parsingTable : ParsingTable<string>)
    (parserFullName : string)
    (stream : Stream)
    : unit =
    let writer = new StreamWriter(stream)

    writeComment writer

    ()


//let generate (eof : 's) (grammar : Grammar<'s>) (parserFullName : string) (stream : Stream) : unit =
//    let automaton = LALR.Automaton.create eof grammar
//
//    let writer = new StreamWriter(stream)
//
//    writeComment writer
//
//    fprintfn writer "module internal rec %s" parserFullName
//    fprintfn writer ""
//
//    writeTerminalType grammar writer
//    fprintfn writer ""
//
//    writeReducerType grammar writer
//    fprintfn writer ""
//
//    writeParseFunction grammar automaton writer
//
//    writer.Flush()
//
//    ()