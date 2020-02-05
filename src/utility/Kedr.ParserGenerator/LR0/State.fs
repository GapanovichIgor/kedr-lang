namespace Kedr.ParserGenerator.LR0

type internal State<'symbol when 'symbol : comparison> = {
    configurations : Configuration<'symbol> Set
    }