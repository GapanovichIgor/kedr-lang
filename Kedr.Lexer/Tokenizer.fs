module Kedr.Tokenizer

open System
open Kedr
open System.IO
open System.Text
        
let parse (encoding : Encoding) (stream : Stream) : Token list =
    let tape = new Tape(stream, encoding)
    
    let parser =
        LongestWinsCombinator
            ([ NumberParser()
               QuotedStringParser() ])
        :> IParser<_,_>
            
    let mutable completeMatchLength = 0
    
    let inline finishRound () =
        let token =
            if completeMatchLength > 0 then
                let chars = tape.RollbackAndConsume(completeMatchLength)
                parser.Parse(chars)
            else
                let chars = tape.RollbackAndConsume(tape.WindowLength)
                InvalidToken(String chars) // TODO: continue parsing invalid token till whitespace or eof
            
        parser.Reset()
        completeMatchLength <- 0
        
        token
    
    [
        while not tape.EndReached do
            let c = tape.GetNext()
            
            match c with
            | Some c ->
                parser.Feed(c) ;assert(parser.Status <> Initial)
                
                match parser.Status with
                | CompleteMatch -> completeMatchLength <- tape.WindowLength
                | FailedMatch ->
                    yield finishRound()
                | _ -> ()
            | None ->
                yield finishRound()
     ]
        
//let parseOld (encoding : Encoding) (stream : Stream) : Token list =
//    let tape = new Tape(stream, encoding)
//
//    let allTokenParsers = getTokenParsers()
//    let mutable aliveTokenParsers = allTokenParsers
//
//    let mutable candidate : IParser<char, Token> option = None
//    let mutable candidateCharCount = 0
//
//    let tokens = new Collections.Generic.List<Token>()
//    
//    let mutable keepGoing = true
//    
//    let inline reset () =
//        candidate <- None
//        for parser in allTokenParsers do
//            parser.Reset()
//        aliveTokenParsers <- allTokenParsers
//        
//    let inline finishRound () =
//        match candidate with
//        | Some parser ->
//            let chars = tape.RollbackAndConsume(candidateCharCount)
//            parser.Parse(chars)
//            |> tokens.Add
//        | None ->
//            if tape.WindowLength > 0 then
//                // todo read invalid token until whitespace or eof
//                tape.RollbackAndConsume(tape.WindowLength)
//                |> String
//                |> InvalidToken
//                |> tokens.Add
//    
//    while keepGoing do
//        let c = tape.GetNext()
//        
//        match c with
//        | None ->
//            finishRound()
//            keepGoing <- false
//        | Some (WhiteSpace _) ->
//            if tape.WindowLength > 1 then
//                finishRound()
//            tape.RollbackAndSkip(1)
//        | Some c ->
//            for parser in aliveTokenParsers do
//                parser.Feed(c)
//
//            aliveTokenParsers <- aliveTokenParsers |> List.filter (fun m -> m.Status <> FailedMatch)
//            
//            if aliveTokenParsers.Length = 0 then
//                finishRound()
//                reset()
//            else
//                let newCandidates =
//                    aliveTokenParsers
//                    |> List.filter (fun p -> p.Status = CompleteMatch)
//                    
//                if newCandidates.Length > 1 then failwith "Ambiguous token"
//                
//                if newCandidates.Length = 1 then
//                    candidate <- Some newCandidates.Head
//                    candidateCharCount <- tape.WindowLength
//    
//    tokens |> List.ofSeq
    
let parseString (str : string) : Token list =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding ms