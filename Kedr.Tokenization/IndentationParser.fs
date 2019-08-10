module internal Kedr.Tokenization.IndentationParser

open Kedr.Parsing
open Kedr.Parsing.Primitives
open WhiteSpace

let [<Literal>] private tabSize = 4

let inline private getNewStyle (chars : char array) =
    if chars.Length = 0 then
        None
    else
        let mixed =
            chars
            |> Seq.pairwise
            |> Seq.exists (fun (x, y) -> x <> y)

        if mixed then Some Mixed
        elif chars.[0] = ' ' then Some Space
        else Some Tab

let parse (tape : Tape<char>, state : TokenizerState) : ParseResult<Token list, TokenizerState, PrimitiveError<char>> =
    let p = zeroOrMoreCond isWhiteSpace

    match p (tape, state) with
    | Ok s ->
        let chars = s.value

        let newLevel =
            chars
            |> Array.sumBy (fun c ->
                match c with
                | ' ' -> 1
                | '\t' -> tabSize
                | _ -> 0)

        let newStyle = getNewStyle chars

        let resultStyle =
            match state.indentationStyle, newStyle with
            | Some oldStyle, Some newStyle ->
                if oldStyle <> newStyle
                then Mixed
                else newStyle
                |> Some
            | Some oldStyle, None -> Some oldStyle
            | None, Some newStyle -> Some newStyle
            | None, None -> None

        let rec resolveLevelsAndTokens levStack tokens =
            match levStack with
            | [] -> ([ newLevel ], tokens)
            | topLevel :: ls ->
                if newLevel > topLevel then
                    (newLevel :: levStack, BlockOpen :: tokens |> List.rev)
                elif newLevel = topLevel then
                    (levStack, SoftBreak :: tokens |> List.rev)
                else
                    resolveLevelsAndTokens ls (BlockClose :: tokens)

        let (resultLevels, tokens) = resolveLevelsAndTokens state.indentationLevels []

        { value = tokens
          state =
            { indentationLevels = resultLevels
              indentationStyle = resultStyle }
          length = s.length }
        |> Ok

    | Error e -> Error e

let terminateWhenEndReached (state : TokenizerState) : Token list * TokenizerState =
    let rec unravel levels tokens =
        match levels with
        | [_] -> tokens
        | _ :: ls -> unravel ls (BlockClose :: tokens)
        | _ -> tokens
    
    let blockEnds = unravel state.indentationLevels []
    
    let state = { state with indentationLevels = [] }
    
    (blockEnds, state)