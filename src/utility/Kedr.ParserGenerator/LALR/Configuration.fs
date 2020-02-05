namespace Kedr.ParserGenerator.LALR

open Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{ToString()}")>]
type internal Configuration<'symbol when 'symbol : comparison> =
    {
        production : Production<'symbol>
        cursorOffset : int
        lookahead : 'symbol Set
    }
    override this.ToString() =
        let str = this.production.ToString()

        let subProd = {
            this.production with
                into = this.production.into |> List.take this.cursorOffset
            }
        let subProdLen = subProd.ToString().Length

        let str = str.Insert(subProdLen, "·")

        sprintf "%s [%s]" str (this.lookahead.ToString())