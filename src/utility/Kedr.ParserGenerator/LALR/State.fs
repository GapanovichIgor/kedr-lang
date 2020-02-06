namespace Kedr.ParserGenerator.LALR

open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type internal State<'symbol when 'symbol : comparison> =
    {
        configurations : Configuration<'symbol> Set
    }
    override this.ToString () =
        this.configurations
        |> Seq.map string
        |> String.concat "   "
        |> sprintf "{ %s }"