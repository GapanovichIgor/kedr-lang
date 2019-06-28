namespace Kedr.Lexer.Tests

open FsCheck

type Token private (token) =
    member val private token = token

    member __.str =
        match token with
        | DecimalNumber n -> n.str
        | StringLiteral s -> s.str

    override this.ToString() =
        match token with
        | DecimalNumber n -> n.ToString()
        | StringLiteral s -> s.ToString()

    static member generator = gen {
        let! token = Arb.generate<Token'>

        return Token token
    }

    static member shrink (value : Token) = seq {
        match value.token with
        | DecimalNumber n ->
            yield!
                DecimalNumberLiteral.shrink n
                |> Seq.map (DecimalNumber >> Token)
        | StringLiteral s ->
            yield!
                StringLiteral.shrink s
                |> Seq.map (StringLiteral >> Token)
    }

    static member arb = Arb.fromGenShrink (Token.generator, Token.shrink)

and private Token' =
    | DecimalNumber of DecimalNumberLiteral
    | StringLiteral of StringLiteral