module internal Kedr.ParserGenerator.StableSorting

open Kedr.ParserGenerator.LALR
open System

let keyOfSymbol symbol =
    symbol.ToString()

let keyOfSymbolString (symbols : _ list) =
    symbols
    |> Seq.map keyOfSymbol
    |> String.Concat

let keyOfProduction (prod : Production<_>) =
    (keyOfSymbol prod.from) + (keyOfSymbolString prod.into)

let keyOfConfiguration (cfg : Configuration<_>) =
    let lookaheadKey =
        cfg.lookahead
        |> Seq.map keyOfSymbol
        |> Seq.sort
        |> String.Concat

    (keyOfProduction cfg.production) + (string cfg.cursorOffset) + lookaheadKey

let keyOfState (state : State<_>) =
    state.configurations
    |> Seq.map keyOfConfiguration
    |> Seq.sort
    |> String.Concat