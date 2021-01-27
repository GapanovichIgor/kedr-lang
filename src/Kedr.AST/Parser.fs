module Kedr.AST.Parser

open Kedr.Tokenization
open ParserImpl

type private T = Token
type private E = Expr

let private toTerminal token =
    match token with
    | T.QuotedString content -> T_strlit content
    | T.Number (i, f) -> T_numlit (i, f)
    | T.Identifier id -> T_id id
    | T.ParenOpen -> T_pareno ()
    | T.ParenClose -> T_parenc ()
    | _ -> failwith "TODO"

let private reducer = {
    EAPP_EAPP_EPAREN = E.Application
    EAPP_EPAREN = id
    EPAREN_ESIMP = id
    EPAREN_pareno_EPAREN_parenc = fun ((), e, ()) -> E.InParens e
    ESIMP_id = E.IdRef
    ESIMP_numlit = E.NumLit
    ESIMP_strlit = E.StrLit
    S_EAPP = id
}

let parse (tokens : seq<Token>) : Result<Expr, string> =
    let input = tokens |> Seq.map toTerminal
    parse reducer input