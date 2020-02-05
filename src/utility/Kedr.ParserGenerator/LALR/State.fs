namespace Kedr.ParserGenerator.LALR

type internal State<'symbol when 'symbol : comparison> = {
    configurations : Configuration<'symbol> Set
    }

