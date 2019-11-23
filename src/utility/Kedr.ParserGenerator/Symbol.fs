namespace Kedr.ParserGenerator
open System

type Symbol = private Symbol of Guid

module Symbol =
    let create () = Symbol (Guid.NewGuid())