namespace Kedr.ParserGenerator.CodeGen

open System
open System.IO
open Kedr.ParserGenerator

type internal ParserDefinition = {
    symbolTypes : Map<string, string>
    productions : Production<string> Set
}

module internal ParserDefinition =
    type private LineData =
        | Production of Production<string>
        | Typing of symbol : string * symbolType : string
        | Blank

    let private parseLine (line : string) =
        let prodParts = line.Split("->")
        if prodParts.Length = 2 then
            let from = prodParts.[0].Trim()
            let into =
                prodParts.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries)
                |> List.ofArray
            Ok (Production { from = from; into = into })
        else
        let typingParts = line.Split(":")
        if typingParts.Length = 2 then
            let symbol = typingParts.[0].Trim()
            let type_ = typingParts.[1].Trim()
            Ok (Typing (symbol, type_))
        else
        if line |> Seq.forall Char.IsWhiteSpace then
            Ok Blank
        else
            Error "Malformed line. Expected a production (A -> B c | D) or a typing (A : T)."

    let parse (stream : Stream) =
        let reader = new StreamReader(stream)

        let parseResult =
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine()
            }
            |> Seq.mapi (fun i line ->
                parseLine line
                |> Result.mapError (fun er -> sprintf "Error at line %i: %s" (i + 1) er))
            |> Result.fromSeqOfResults

        match parseResult with
        | Error lineErrors ->
            lineErrors
            |> String.concat Environment.NewLine
            |> Error
        | Ok lines ->
            let symbolTypes =
                lines
                |> Seq.choose (function
                    | Typing (s, t) -> Some (s, t)
                    | _ -> None)
                |> Map.ofSeq

            let productions =
                lines
                |> List.choose (function
                    | Production p -> Some p
                    | _ -> None)
                |> Set.ofSeq

            if productions.IsEmpty then Error "No productions defined"
            else

            let symbolsInProductions = productions |> Seq.collect Production.getSymbols

            let untypedSymbols =
                symbolsInProductions
                |> Seq.filter (symbolTypes.ContainsKey >> not)
                |> Set.ofSeq

            if not untypedSymbols.IsEmpty then
                let symbolList = untypedSymbols |> String.concat ", "
                Error (sprintf "Missing type for symbols %s" symbolList)
            else
                Ok { symbolTypes = symbolTypes; productions = productions }