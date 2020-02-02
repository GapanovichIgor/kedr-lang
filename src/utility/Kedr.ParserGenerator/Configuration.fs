namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type Configuration = {
    production : Production
    cursorOffset : int
    }
    with
    static member private interpunct = Symbol("·")
    override this.ToString() =
        let p = {
            this.production with
                into =
                    this.production.into
                    |> List.insert Configuration.interpunct this.cursorOffset
            }

        p.ToString()

module Configuration =
    let tryAheadSymbol configuration =
        if configuration.production.into.Length > configuration.cursorOffset
        then configuration.production.into.[configuration.cursorOffset] |> Some
        else None

    let createStart (production : Production) : Configuration =
        { production = production
          cursorOffset = 0 }

    let rec close
        (productions : Production Set)
        (configuration : Configuration)
        : Configuration list =
        [
            // if configuration is A -> α.Bω where B is non-terminal then
            // get all productions of B at start configuration (B -> .γ)
            if configuration.production.into.Length > configuration.cursorOffset then
                let symbolToExpand = configuration.production.into.[configuration.cursorOffset]
                let symbolToExpandProductions = productions |> Set.filter (fun prod -> prod.from = symbolToExpand)
                let unusedProductions = productions - symbolToExpandProductions
                for prod in symbolToExpandProductions do
                    let startConfiguration = createStart prod
                    yield startConfiguration
                    yield! close unusedProductions startConfiguration
        ]