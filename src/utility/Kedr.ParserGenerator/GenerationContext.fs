namespace Kedr.ParserGenerator

open Kedr.ParserGenerator.LALR

type internal GenerationContext<'a when 'a : comparison> = {
    grammar : Grammar<'a>
    automaton : Automaton<'a>
    parsingTable : ParsingTable<'a>
}

