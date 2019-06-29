namespace Kedr.Parser

open Kedr.Parser.Util

type private State = Nothing | AtEmptyIntegerPart | AtIntegerPart | AtEmptyFractionalPart | AtFractionalPart

type internal NumberLiteralMatcher() =

    let ssNothing = (Nothing, Initial)
    let ssAtEmptyIntegerPart = (AtEmptyIntegerPart, PartialMatch)
    let ssAtIntegerPart = (AtIntegerPart, CompleteMatch)
    let ssAtEmptyFractionalPart = (AtEmptyFractionalPart, PartialMatch)
    let ssAtFractionalPart = (AtFractionalPart, CompleteMatch)

    let mutable stateStatus = ssNothing

    interface IMatcher with
        override val Parser = Some Number
        override __.Status = snd stateStatus
        override __.Reset() = stateStatus <- ssNothing
        override __.Feed(c : char) =
            let state = fst stateStatus
            stateStatus <-
                match state with
                | Nothing ->
                    match c with
                    | '+' | '-' -> ssAtEmptyIntegerPart
                    | Digit -> ssAtIntegerPart
                    | _ -> (state, FailedMatch)
                | AtEmptyIntegerPart ->
                    match c with
                    | Digit -> ssAtIntegerPart
                    | _ -> (state, FailedMatch)
                | AtIntegerPart ->
                    match c with
                    | '.' -> ssAtEmptyFractionalPart
                    | Digit -> ssAtIntegerPart
                    | _ -> (state, FailedMatch)
                | AtEmptyFractionalPart
                | AtFractionalPart ->
                    match c with
                    | Digit -> ssAtFractionalPart
                    | _ -> (state, FailedMatch)