open System
open System.IO
open Kedr.ParserGenerator

let eof = "$"

let isValidSymbolName (s : string) =
    s |> Seq.forall (fun c ->
        Char.IsLetterOrDigit c ||
        c = '_')

let collectResults rs =
    rs |> Seq.fold (fun state el ->
        match state, el with
        | Ok state, Ok el -> Ok (el :: state)
        | Error state, Error el -> Error (el :: state)
        | Ok _, Error el -> Error [el]
        | Error state, Ok _ -> Error state)
        (Ok [])

let parseLine (line : string) =
    let invalidSymbolName s = Error (sprintf "invalid symbol name '%s'" s)

    match line.Split("->") with
    | [| from; into |] ->
        let from = from.Trim()
        if not (isValidSymbolName from) then invalidSymbolName from
        else

        let intoCases = into.Split('|')
        if intoCases |> Seq.exists String.IsNullOrWhiteSpace then Error "empty production case"
        else

        seq {
            for case in intoCases do
                let intoSymbols = case.Split(' ', StringSplitOptions.RemoveEmptyEntries)

                let invalidIntoSymbol = intoSymbols |> Array.tryFind (isValidSymbolName >> not)
                match invalidIntoSymbol with
                | Some s -> invalidSymbolName s
                | None ->
                    Ok { from = from; into = List.ofArray intoSymbols }
        }
        |> collectResults
        |> Result.mapError (fun errors -> errors |> List.head)

    | _ -> Error "missing '->'"

[<EntryPoint>]
let main argv =
    if argv.Length < 2 then printf "path to grammar and path to output required"; 1
    else

    let grammarFilePath = argv.[0]
    let outputFilePath = argv.[1]

    let lines = File.ReadAllLines grammarFilePath

    if lines.Length = 0 then printf "empty grammar file"; 1
    else

    let productions =
        lines
        |> Seq.mapi (fun i line ->
            parseLine line
            |> Result.mapError (fun error ->
                sprintf "Error at line %i: %s" i error))
        |> collectResults
        |> Result.map (Seq.collect id >> Set.ofSeq)

    match productions with
    | Error errors ->
        errors |> Seq.rev |> Seq.iter Console.WriteLine
        1
    | Ok productions ->
        let grammar = Grammar.fromProductions productions
        use outputFile = File.OpenWrite outputFilePath
        Generator.generate eof grammar outputFile
        0