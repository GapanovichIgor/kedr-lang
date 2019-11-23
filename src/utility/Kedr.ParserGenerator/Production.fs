namespace Kedr.ParserGenerator

type Production = {
    from : Symbol
    into : Symbol list
    }

[<AutoOpen>]
module ProductionOp =
    let (=>) from into = { from = from; into = into }