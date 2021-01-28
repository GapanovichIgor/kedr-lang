namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type Production<'symbol> =
    {
        from : 'symbol
        into : 'symbol list
    }
    override this.ToString() =
        let result =
            this.into
            |> Seq.map (fun s -> s.ToString())
            |> String.concat " "

        sprintf "%s -> %s" (this.from.ToString()) result

module Production =
    let getSymbols prod =
        seq {
            yield prod.from
            yield! prod.into
        }