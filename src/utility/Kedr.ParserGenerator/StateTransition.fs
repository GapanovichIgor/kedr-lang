namespace Kedr.ParserGenerator

type StateTransition<'symbol when 'symbol : comparison> = {
    sourceState : State<'symbol>
    symbol : 'symbol
    destinationState : State<'symbol>
    }