module Kedr.Tokenizer.Tests.Util
open FsCheck
open System

let escapeChar c =
    match c with
    | '\\' -> "\\\\"
    | ' ' -> "\\s"
    | '\t' -> "\\t"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | c -> string c

let escapeString s =
    s
    |> Seq.map escapeChar
    |> String.Concat
    
let (==) x y = x = y |@ sprintf "%A = %A" x y