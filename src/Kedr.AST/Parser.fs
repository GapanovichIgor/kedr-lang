module Kedr.AST.Parser

open Kedr.Tokenization
open ParserImpl

let private toTerminal token =
    match token with
    | QuotedString content -> T_strlit content
    | Number (i, f) -> T_numlit (i, f)
    | Identifier id -> T_id id
    | ParenOpen -> T_pareno ()
    | ParenClose -> T_parenc ()
    | _ -> failwith "TODO"

let private reducer = {
    EXPR_pareno_EXPR_parenc = fun (_, expr, _) -> expr
    EXPR_id = fun id -> Expr.IdRef id
    EXPR_numlit = fun num -> Expr.NumLit num
    EXPR_strlit = fun content -> Expr.StrLit content
    S_EXPR = fun expr -> expr
}

let parse (tokens : seq<Token>) =
    let input = tokens |> Seq.map toTerminal
    parse reducer input