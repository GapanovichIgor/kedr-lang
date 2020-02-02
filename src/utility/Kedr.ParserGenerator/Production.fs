namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type Production = {
    from : Symbol
    into : Symbol list
    }
    with
    override this.ToString() =
        let result =
            this.into
            |> Seq.map string
            |> String.concat " "

        sprintf "%s 🠚 %s" (string this.from) result

[<AutoOpen>]
module ProductionOp =
    let (=>) from into = { from = from; into = into }