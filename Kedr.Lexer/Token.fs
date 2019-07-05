namespace Kedr

type Token =
    | InvalidToken of string
    | Minus
    | Plus
    | NumberLiteral of integerPart:int64 * fractionalPart: int64 option
    | StringLiteral of contents:string