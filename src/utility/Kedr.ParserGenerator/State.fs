namespace Kedr.ParserGenerator

type State<'symbol when 'symbol : comparison> = {
    configurations : Configuration<'symbol> Set
    }