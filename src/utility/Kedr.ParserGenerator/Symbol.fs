namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{Item}")>]
type Symbol =
    Symbol of string
    with
    override this.ToString() = let (Symbol str) = this in str