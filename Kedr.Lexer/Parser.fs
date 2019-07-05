module Kedr.Parser

open Kedr
open System
open System.IO
open System.Text
open Utils

let private getTokenParsers () : IParser<char, Token> list = [
    NumberParser()
    StringLiteralParser()
]

type private Tape(stream : Stream, encoding : Encoding) =
    let reader = new StreamReader(stream, encoding)

    let mutable buffer = Array.zeroCreate<char>(4096)
    let mutable bufferStart = 0
    let mutable bufferLength = 0
    let mutable headPos = -1

    member __.WindowLength =
        assert (headPos >= bufferStart)
        headPos - bufferStart + 1

    member __.GetNext() =
        headPos <- headPos + 1
        if headPos = bufferStart + bufferLength then
            let i = reader.Read()
            if i = -1 then
                None
            else
                let c = char i

                if bufferStart + bufferLength = buffer.Length then
                    if bufferStart > 0 then
                        Array.Copy(buffer, bufferStart, buffer, 0, bufferLength)
                    else
                        let newBuffer = Array.zeroCreate<char>(buffer.Length * 2)
                        Array.Copy(buffer, bufferStart, newBuffer, 0, bufferLength)
                        buffer <- newBuffer

                bufferLength <- bufferLength + 1
                buffer.[headPos] <- c

                Some c
        else
            let c = buffer.[headPos]
            Some c

    member this.RollbackAndConsume(n : int) : char[] =
        assert (n <= bufferLength)
        assert (bufferStart + n <= buffer.Length)
        
        let chars = buffer |> getSubArray bufferStart n
        
        this.RollbackAndSkip(n)
        
        chars
        
    member __.RollbackAndSkip(n : int) : unit =
        assert (n <= bufferLength)
        assert (bufferStart + n <= buffer.Length)
        
        bufferStart <- bufferStart + n
        bufferLength <- bufferLength - n
        headPos <- bufferStart - 1
        
let parse (encoding : Encoding) (stream : Stream) : Token list =
    let tape = new Tape(stream, encoding)

    let allTokenParsers = getTokenParsers()
    let mutable aliveTokenParsers = allTokenParsers

    let mutable candidate : IParser<char, Token> option = None
    let mutable candidateCharCount = 0

    let tokens = new Collections.Generic.List<Token>()
    
    let mutable keepGoing = true
    
    let inline reset () =
        candidate <- None
        for parser in allTokenParsers do
            parser.Reset()
        aliveTokenParsers <- allTokenParsers
        
    let inline finishRound () =
        match candidate with
        | Some parser ->
            let chars = tape.RollbackAndConsume(candidateCharCount)
            parser.Parse(chars)
            |> tokens.Add
        | None ->
            if tape.WindowLength > 0 then
                // todo read invalid token until whitespace or eof
                tape.RollbackAndConsume(tape.WindowLength)
                |> String
                |> InvalidToken
                |> tokens.Add
    
    while keepGoing do
        let c = tape.GetNext()
        
        match c with
        | None ->
            finishRound()
            keepGoing <- false
        | Some (WhiteSpace _) ->
            if tape.WindowLength > 1 then
                finishRound()
            tape.RollbackAndSkip(1)
        | Some c ->
            for matcher in aliveTokenParsers do
                matcher.Feed(c)

            aliveTokenParsers <- aliveTokenParsers |> List.filter (fun m -> m.Status <> FailedMatch)
            
            if aliveTokenParsers.Length = 0 then
                finishRound()
                reset()
            else
                let newCandidates =
                    aliveTokenParsers
                    |> List.filter (fun p -> p.Status = CompleteMatch)
                    
                if newCandidates.Length > 1 then failwith "Ambiguous token"
                
                if newCandidates.Length = 1 then
                    candidate <- Some newCandidates.Head
                    candidateCharCount <- tape.WindowLength
    
    tokens |> List.ofSeq

let private parseOld (encoding : Encoding) (stream : Stream) : Token list =
    let tape = new Tape(stream, encoding)

    let allTokenParsers = getTokenParsers()
    let mutable aliveTokenParsers = allTokenParsers

    let mutable candidate : IParser<char, Token> option = None
    let mutable candidateCharCount = 0

    let tokens = new Collections.Generic.List<Token>()

    let rec loop () =
        let finishRound finished =
            let token = 
                match candidate with
                | Some parser ->
                    let chars = tape.RollbackAndConsume(candidateCharCount)
                    parser.Parse(chars)
                    |> Some
                | None ->
                    match tape.WindowLength with
                    | 0 -> None
                    | _ ->
                        tape.RollbackAndConsume(tape.WindowLength)
                        |> String
                        |> InvalidToken
                        |> Some
                            
            match token with
            | Some token -> tokens.Add(token)
            | None -> ()
                    
            if not finished then
                candidate <- None
                for matcher in allTokenParsers do
                    matcher.Reset()
                aliveTokenParsers <- allTokenParsers
                loop()

        let c = tape.GetNext()

        match c with
        | None -> finishRound true
        | Some c ->
            for matcher in aliveTokenParsers do
                matcher.Feed(c)

            aliveTokenParsers <- aliveTokenParsers |> List.filter (fun m -> m.Status <> FailedMatch)

            if aliveTokenParsers.Length = 0 then
                finishRound false
            else
                let newCandidate =
                    aliveTokenParsers
                    |> Seq.filter (fun p -> p.Status = CompleteMatch)
                    |> Seq.tryExactlyOne
                
                match newCandidate with
                | Some newCandidate ->
                    candidate <- Some newCandidate
                    candidateCharCount <- tape.WindowLength
                | None -> ()

                loop()

    loop()
    
    tokens |> List.ofSeq

let parseString (str : string) : Token list =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding ms