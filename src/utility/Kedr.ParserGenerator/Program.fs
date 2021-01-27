open System
open System.IO
open Kedr.ParserGenerator
open Kedr.ParserGenerator.CodeGen
open Kedr.ParserGenerator.CodeGen.CodeGenerator
open Kedr.ParserGenerator.LALR

let eof = "$"

[<EntryPoint>]
let main argv =
    if argv.Length < 3 then printf "arguments required: <path to definition file> <path to output file> <module full name>"; 1
    else

    let definitionFilePath = argv.[0]
    let outputFilePath = argv.[1]
    let moduleFullName = argv.[2]

    use definitionFile = File.OpenRead definitionFilePath

    let parserDefinition = ParserDefinition.parse definitionFile

    match parserDefinition with
    | Error error ->
        Console.WriteLine error
        1
    | Ok parserDefinition ->
        let grammar = Grammar.fromProductions parserDefinition.productions
        let parsingTable =
            Automaton.create eof grammar
            |> ParsingTable.create

        let args =
            { newLine = Environment.NewLine
              eofSymbol = eof
              symbolTypes = parserDefinition.symbolTypes
              symbolToIdentifier = id
              parsingTable = parsingTable
              parserModuleName = moduleFullName }

        use outputFile = File.Create outputFilePath

        generate args outputFile
        0