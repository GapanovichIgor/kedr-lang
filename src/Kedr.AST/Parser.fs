module Kedr.AST.Parser

open Kedr.Tokenization
open ParserImpl

type private T = Token

let private toTerminal token =
    match token with
    | T.QuotedString content -> T_strlit content
    | T.Number (i, f) -> T_numlit (i, f)
    | T.Identifier id -> T_id id
    | T.ParenOpen -> T_pareno
    | T.ParenClose -> T_parenc
    | T.Let -> T_let
    | T.Equals -> T_eq
    | T.Colon -> T_colon
    | _ -> failwith "TODO"

let private reducer = {
    BINDPARAMS_ = []
    BINDPARAMS_BINDPARAMS_BINDPARAM = fun (bindparams, bindparam) -> bindparam :: bindparams
    BINDPARAM_id = fun name ->
        { name = name
          typeAnnotation = None }
    BINDPARAM_pareno_id_colon_id_parenc = fun (name, type_) ->
        { name = name
          typeAnnotation = Some type_ }
    BIND_let_id_BINDPARAMS_TYPEANNOT_eq_EXPR = fun (name, parameters, typeAnnotation, body) ->
        { name = name
          parameters = parameters |> List.rev
          typeAnnotation = typeAnnotation
          body = body }
    PROGRAM_BIND = PBinding
    PROGRAM_EXPR = PExpr
    TYPEANNOT_ = None
    TYPEANNOT_colon_id = Some
    EAPP_EAPP_EPAREN = Application
    EAPP_EPAREN = id
    EPAREN_ESIMP = id
    EPAREN_pareno_EPAREN_parenc = id
    ESIMP_id = IdRef
    ESIMP_numlit = NumLit
    ESIMP_strlit = StrLit
    EXPR_EAPP = id
}

let parse (tokens : seq<Token>) : Result<Program, string> =
    let input = tokens |> Seq.map toTerminal
    parse reducer input