namespace Kedr.ParserGenerator.CodeGen

open System
open System.IO
open System.Text.RegularExpressions
open Kedr.ParserGenerator

type internal ParserDefinition = {
    symbolTypes : Map<string, string>
    productions : Production<string> Set
}

module internal ParserDefinition =
    type private LineData =
        | Productions of Production<string> list
        | Typing of symbol : string * symbolType : string
        | Blank

    let private typingRegex = Regex("^\s*(?<symbol>[A-Za-z]+)\s*:\s*(?<type>.+?)\s*$")
    let private (|AsTyping|_|) (line : string) =
        let m = typingRegex.Match(line)
        if m.Success then
            let symbol = m.Groups.["symbol"].Value
            let type_ = m.Groups.["type"].Value
            Some (Typing (symbol, type_))
        else
            None

    let private productionRegex = Regex("^\s*(?<symbol>[A-Z]+)\s*->\s*(?<into>.+?)\s*$")
    let private (|AsProduction|_|) (line : string) =
        let m = productionRegex.Match(line)
        if m.Success then
            let symbol = m.Groups.["symbol"].Value
            let into = m.Groups.["into"].Value
            let intoAlternatives =
                into.Split('|')
                |> Seq.map (fun i -> i.Trim().Split(' '))

            if intoAlternatives |> Seq.exists Seq.isEmpty then None else

            let validSymbol text = text |> Seq.forall (fun c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')

            if intoAlternatives |> Seq.exists (Seq.exists (not << validSymbol)) then None else

            intoAlternatives
            |> Seq.map (fun intoSymbols ->
                { from = symbol
                  into = intoSymbols |> List.ofArray })
            |> List.ofSeq
            |> Productions
            |> Some
        else
            None

    let private (|AsBlankLine|_|) (line : string) =
        if line |> Seq.forall Char.IsWhiteSpace
        then Some ()
        else None

    let private parseLine (line : string) =
        match line with
        | AsProduction p -> Ok p
        | AsTyping t -> Ok t
        | AsBlankLine -> Ok Blank
        | _ -> Error "Malformed line. Expected a production (A -> B c | D) or a typing (A : T)."

    let parse (stream : Stream) =
        let reader = new StreamReader(stream)

        let parseResult =
            seq {
                while not reader.EndOfStream do
                    reader.ReadLine()
            }
            |> Seq.map parseLine
            |> Seq.mapi (fun i res -> res |> Result.mapError (fun er -> sprintf "Error at line %i: %s" (i + 1) er))
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
                |> Seq.choose (function
                    | Productions p -> Some p
                    | _ -> None)
                |> Seq.collect id
                |> Set.ofSeq

            if productions.IsEmpty then Error "No productions defined" else

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