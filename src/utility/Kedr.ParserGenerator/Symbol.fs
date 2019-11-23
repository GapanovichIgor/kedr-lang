namespace Kedr.ParserGenerator
open System.Diagnostics

[<DebuggerDisplay("{Item}")>]
type Symbol =
    private Symbol of string
    with
    override this.ToString() = let (Symbol str) = this in str

module Symbol =
    let create name = Symbol name