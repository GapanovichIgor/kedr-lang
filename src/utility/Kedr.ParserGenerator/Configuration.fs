namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type Configuration = {
    production : Production
    cursorOffset : int
    }
    with
    static member private interpunct = Symbol.create "·"
    override this.ToString() =
        let p = {
            this.production with
                into =
                    this.production.into
                    |> List.insert Configuration.interpunct this.cursorOffset
            }

        p.ToString()

