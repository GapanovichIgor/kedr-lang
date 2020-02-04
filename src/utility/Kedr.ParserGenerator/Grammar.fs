namespace Kedr.ParserGenerator

type Grammar =
    private {
        _productions : Production Set
        _symbols : Symbol Set
        _terminals : Symbol Set
        _nonTerminals : Symbol Set
        _startingSymbols : Symbol Set
    }
    member this.productions = this._productions
    member this.symbols = this._symbols
    member this.terminals = this._terminals
    member this.nonTerminals = this._nonTerminals
    member this.startingSymbols = this._startingSymbols

module Grammar =
    let fromProductions productions =
        let nonTerminals = productions |> Set.map (fun p -> p.from)
        let producedSymbols = productions |> Seq.collect (fun p -> p.into) |> Set.ofSeq
        let terminals = producedSymbols - nonTerminals
        let startingSymbols = nonTerminals - producedSymbols

        if startingSymbols.IsEmpty then failwith "Invalid grammar: no starting symbols"

        {
            _productions = productions
            _symbols = terminals + nonTerminals
            _terminals = terminals
            _nonTerminals = nonTerminals
            _startingSymbols = startingSymbols
        }