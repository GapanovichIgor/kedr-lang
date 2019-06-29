module private Kedr.Parser.Util

open System

let (|Digit|_|) c =
    if Char.IsDigit c then Some ()
    else None