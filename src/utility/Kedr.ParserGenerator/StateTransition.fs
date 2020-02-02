namespace Kedr.ParserGenerator

type StateTransition = {
    sourceState : State
    symbol : Symbol
    destinationState : State
    }