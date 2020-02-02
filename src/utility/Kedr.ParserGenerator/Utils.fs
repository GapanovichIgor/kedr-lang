namespace Kedr.ParserGenerator
open System

module List =
    let insert x pos list =
        let rec insert' x pos head tail =
            if pos < 0 then
                raise (IndexOutOfRangeException())
            elif pos = 0 then
                if head |> List.isEmpty
                then x :: tail
                else head @ (x :: tail)
            else
                insert' x (pos - 1) (head @ [ tail.Head ]) tail.Tail

        insert' x pos [] list

[<AutoOpen>]
module Prelude =
    type OptionBuilder() =
        member __.Bind(opt, f) = Option.bind f opt
        member __.Return(x) = Some x

    let option = OptionBuilder()