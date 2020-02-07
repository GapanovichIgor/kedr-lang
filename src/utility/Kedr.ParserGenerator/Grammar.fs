namespace Kedr.ParserGenerator

type Grammar<'symbol when 'symbol : comparison> =
    private {
        productions : Production<'symbol> Set
        symbols : 'symbol Set
        terminals : 'symbol Set
        nonTerminals : 'symbol Set
        startingSymbols : 'symbol Set
    }

module Grammar =
    let fromProductions productions =
        let nonTerminals = productions |> Set.map (fun p -> p.from)
        let producedSymbols = productions |> Seq.collect (fun p -> p.into) |> Set.ofSeq
        let terminals = producedSymbols - nonTerminals
        let startingSymbols = nonTerminals - producedSymbols

        if startingSymbols.IsEmpty then failwith "Invalid grammar: no starting symbols"

        {
            productions = productions
            symbols = terminals + nonTerminals
            terminals = terminals
            nonTerminals = nonTerminals
            startingSymbols = startingSymbols
        }