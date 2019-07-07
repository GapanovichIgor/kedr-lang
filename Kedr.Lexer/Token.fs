namespace Kedr

type Token =
    | InvalidToken of string
    | Minus
    | Plus
    | Asterisk
    | Slash
    | Equals
    | NotEquals
    | ParenOpen
    | ParenClose
    | Number of integerPart:int64 * fractionalPart: int64 option
    | QuotedString of contents:string