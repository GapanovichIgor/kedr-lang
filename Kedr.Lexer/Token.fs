namespace Kedr.Parser

type Token =
    | Number of string
    | StringLiteral of contents:string