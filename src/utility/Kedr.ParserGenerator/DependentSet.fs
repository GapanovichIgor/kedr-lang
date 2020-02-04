namespace Kedr.ParserGenerator

open System.Collections.Generic

type private DependentSet<'a when 'a : comparison> () =
    let singleInclusions = HashSet()
    let subSets = HashSet()

    member _.Add(item : 'a) =
        singleInclusions.Add(item) |> ignore

    member _.Add(subSet : DependentSet<'a>) =
        subSets.Add(subSet) |> ignore

    member this.ToSet() =
        this.ToSetInternal []

    member private this.ToSetInternal visited =
        if visited |> List.contains this then Set.empty
        else

        let visited = this :: visited
        let ofSubSets =
            subSets
            |> Seq.map (fun s -> s.ToSetInternal(visited))
            |> Seq.fold (+) Set.empty
        let ofSingleInclusions = singleInclusions |> Set.ofSeq

        ofSubSets + ofSingleInclusions