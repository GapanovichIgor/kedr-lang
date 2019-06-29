module Kedr.Parser

open System
open System.IO
open System.Text

open Kedr.Parser

let private matchers : IMatcher list = [
    WhitespaceMatcher()
    NumberLiteralMatcher()
    StringLiteralMatcher()
]

type private Tape(stream : Stream, encoding : Encoding) =
    let reader = new StreamReader(stream, encoding)

    let mutable buffer = Array.zeroCreate<char>(4096)
    let mutable bufferStart = 0
    let mutable bufferLength = 0
    let mutable headPos = -1

    member __.PeekLength = headPos - bufferStart + 1

    member __.PeekNext() =
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

    member __.RollbackAndConsume(n : int) : string =
        let str = new String(buffer, bufferStart, n)
        bufferStart <- bufferStart + n
        headPos <- bufferStart - 1
        str



type TokenParseResult = Token of Token | Failure

let parse (encoding : Encoding) (stream : Stream) : TokenParseResult seq =
    let tape = new Tape(stream, encoding)

    let mutable aliveMatchers = matchers

    let mutable candidate : IMatcher option = None
    let mutable candidateCharCount = 0

    let rec getNextToken () =
        let inline finishRound () =
            match candidate with
            | Some matcher ->
                candidate <- None
                aliveMatchers <- matchers

                match matcher.Parser with
                | Some parser ->
                    let str = tape.RollbackAndConsume(candidateCharCount)
                    let token = parser str
                    Some (Token token)
                | None -> getNextToken()
            | None ->
                match tape.PeekLength with
                | 0 -> None
                | _ -> Some Failure

        let c = tape.PeekNext()

        match c with
        | None -> finishRound()
        | Some c ->
            for matcher in aliveMatchers do
                matcher.Feed(c)

            aliveMatchers <- aliveMatchers |> List.filter (fun m -> m.Status <> FailedMatch)

            if aliveMatchers.Length = 0 then
                finishRound()
            else
                let newCandidates = aliveMatchers |> List.filter (fun p -> p.Status = CompleteMatch)
                if newCandidates.Length > 1 then
                    Some Failure
                else
                    if newCandidates.Length = 1 then
                        candidate <- Some newCandidates.Head
                        candidateCharCount <- tape.PeekLength

                    getNextToken()

    let generate finished =
        if finished then
            None
        else
            match getNextToken() with
            | Some result ->
                match result with
                | Token _ -> Some (result, false)
                | Failure -> Some (result, true)
            | None -> None

    Seq.unfold generate false

let parseString (str : string) : TokenParseResult seq =
    let encoding = Encoding.UTF8
    let ms = new MemoryStream(encoding.GetBytes str)
    parse encoding ms