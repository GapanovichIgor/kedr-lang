module rec Kedr.AST.ASTParser

open Kedr.Tokenization

type Error =
    | TokenSequenceWasEmpty
    | CouldNotReduceToStartSymbol
    | TokensLeftInParseTree
    | InvalidParseTreeNode

type private T = Token

type private ParseTree =
    | Token of Token
    | ValueExpr of ValueAtom
    | QuotedString of string
    | Identifier of string
    | Expression of ParseTree
    | PostfixApplication of fn : ParseTree * arg : ParseTree

type private P = ParseTree

let private reduceProduction symbols =
    match symbols with
    | [ P.Token (T.QuotedString s) ] -> ValueAtom.StringLiteral s |> P.ValueExpr |> Some
    | [ P.Token (T.Identifier i) ] -> ValueAtom.IdentifierRef P.Identifier i |> Some
    | [ P.QuotedString s ] -> P.QuotedString s |> P.Expression |> Some
    | [ P.Identifier i ] -> P.Identifier i |> P.Expression |> Some
    | [ P.PostfixApplication (fn, arg) ] -> P.PostfixApplication (fn, arg) |> P.Expression |> Some
    | [ P.Expression e1; P.Expression e2 ] -> P.PostfixApplication (P.Expression e1, P.Expression e2) |> Some 
    | _ -> None

let rec private reduceLongestTail (f : 'a list -> 'a option) (list : 'a list) : 'a list option =
    match list with
    | [] -> None
    | x :: xs ->
        match f list with
        | Some result -> Some [ result ]
        | None ->
            reduceLongestTail f xs
            |> Option.map (fun tailResult -> x :: tailResult)

let private getParseTree (tokens : Token seq) =
    use enumerator = tokens.GetEnumerator()

    let rec go symbols =
        match reduceLongestTail reduceProduction symbols with
        | Some symbols -> go symbols
        | None ->
            if enumerator.MoveNext()
            then go (symbols @ [ Token (enumerator.Current) ])
            else symbols

    if not (enumerator.MoveNext()) then
        Error TokenSequenceWasEmpty
    else
        match go [ P.Token (enumerator.Current) ] with
        | [ pTree ] -> Ok pTree
        | _ -> Error CouldNotReduceToStartSymbol

let rec private parseTreeToAST pTree =
    match pTree with
    | P.Token _ -> Error TokensLeftInParseTree
    | P.QuotedString s -> Ok (ValueAtom.StringLiteral s)
    | P.Identifier i -> Ok (ValueAtom.IdentifierRef i)
    | P.Expression p -> parseTreeToAST p
    | P.PostfixApplication (P.Expression p1, P.Expression p2) ->
        parseTreeToAST p1 |> Result.bind (fun p1 ->
            parseTreeToAST p2 |> Result.map (fun p2 ->
                ValueExpr.FunctionApplication (p1, p2)))
    | _ -> Error InvalidParseTreeNode

let parse (tokens : Token seq) : Result<ValueAtom, Error> =
    let parseTree = getParseTree tokens

    let ast = parseTree |> Result.bind parseTreeToAST

    match ast with
    | Ok ast -> Ok ast
    | Error e -> Error e