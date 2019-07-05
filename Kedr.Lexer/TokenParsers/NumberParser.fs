namespace Kedr

open System
open System
open System.Collections.Generic
open Kedr.Utils

type private NumberParserState = Nothing | AtIntegerPart | AtEmptyFractionalPart | AtFractionalPart

type internal NumberParser() =
    let ssNothing = (Nothing, Initial)
    let ssAtIntegerPart = (AtIntegerPart, CompleteMatch)
    let ssAtEmptyFractionalPart = (AtEmptyFractionalPart, PartialMatch)
    let ssAtFractionalPart = (AtFractionalPart, CompleteMatch)

    let mutable stateStatus = ssNothing

    interface IParser<char, Token> with
        override __.Status = snd stateStatus
        
        override __.Reset() = stateStatus <- ssNothing
        
        override __.Parse(chars : char array) =
            let mutable separatorInd = chars.Length - 1
            while separatorInd >= 0 && chars.[separatorInd] <> '.' do
                separatorInd <- separatorInd - 1
            
            if separatorInd <> -1 then
                let integerPart =
                    chars
                    |> getSubArray 0 separatorInd
                    |> String
                    |> Int64.Parse
                    
                let fractionalPart =
                    chars
                    |> getSubArray (separatorInd + 1) (chars.Length - separatorInd - 1)
                    |> String
                    |> Int64.Parse
                    
                Number (integerPart, (Some fractionalPart))
            else
                let integerPart =
                    chars
                    |> String
                    |> Int64.Parse
                    
                Number (integerPart, None)
                
        override __.Feed(c : char) =
            assert(snd stateStatus <> FailedMatch)
            
            let state = fst stateStatus
            stateStatus <-
                match state with
                | Nothing ->
                    match c with
                    | Digit _ -> ssAtIntegerPart
                    | _ -> (state, FailedMatch)
                | AtIntegerPart ->
                    match c with
                    | '.' -> ssAtEmptyFractionalPart
                    | Digit _ -> ssAtIntegerPart
                    | _ -> (state, FailedMatch)
                | AtEmptyFractionalPart
                | AtFractionalPart ->
                    match c with
                    | Digit _ -> ssAtFractionalPart
                    | _ -> (state, FailedMatch)