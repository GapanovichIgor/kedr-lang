namespace Kedr

type Token =
    | Let
    | Type
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Equals
    | NotEquals
    | ParenOpen
    | ParenClose
    | Identifier of string
    | Number of integerPart:int64 * fractionalPart: int64 option
    | QuotedString of contents:string
    | InvalidToken of string
