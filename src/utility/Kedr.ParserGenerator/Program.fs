open System
open System.IO
open Kedr.ParserGenerator
open Kedr.ParserGenerator.CodeGen
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
        error |> Console.WriteLine
        1
    | Ok parserDefinition ->
        let grammar = Grammar.fromProductions parserDefinition.productions
        let automaton = Automaton.create eof grammar
        let parsingTable = ParsingTable.create automaton
        use outputFile = File.Create outputFilePath
        CodeGenerator.generate eof parserDefinition parsingTable moduleFullName outputFile
        0