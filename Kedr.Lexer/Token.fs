namespace Kedr

type Token =
    | InvalidToken of string
    | Minus
    | Plus
    | Asterisk
    | Slash
    | Number of integerPart:int64 * fractionalPart: int64 option
    | QuotedString of contents:string