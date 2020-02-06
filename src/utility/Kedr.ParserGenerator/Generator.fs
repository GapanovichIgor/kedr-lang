module Kedr.ParserGenerator.Generator

open System.IO

let generate (eof : 's) (grammar : Grammar<'s>) (stream : Stream) : unit =
    let automaton = LALR.Automaton.create eof grammar

    failwith ""