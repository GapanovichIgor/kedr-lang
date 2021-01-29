namespace Kedr.Tokenization

type Token =
    | Let
    | Type
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Equals
    | NotEquals
    | Colon
    | ParenOpen
    | ParenClose
    | SquareBracketOpen
    | SquareBracketClose
    | Identifier of string
    | Number of integerPart:uint32 * fractionalPart: uint32 option
    | QuotedString of contents:string
    | BlockOpen
    | SoftBreak
    | HardBreak
    | BlockClose
    | InvalidToken of string
