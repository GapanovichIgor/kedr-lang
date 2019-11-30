namespace Kedr.ParserGenerator

type Bag<'a when 'a : comparison> = private Bag of Map<'a, 'a>

module Bag =
    let empty = Bag (Map.empty)

    let add item bag =
        let (Bag map) = bag
        let map = map |> Map.add item item
        Bag map

    let singleton item = empty |> add item

    let getExistingOrAdd item bag =
        let (Bag map) = bag
        match map |> Map.tryFind item with
        | Some existing -> (existing, bag)
        | None ->
            let bag = Bag (map |> Map.add item item)
            (item, bag)
