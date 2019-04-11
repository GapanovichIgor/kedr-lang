namespace Kedr.Lexer.Tests

open FsCheck

type Token private (token) =
    member val private token = token

    member __.str =
        match token with
        | DecimalNumber n -> n.str

    override this.ToString() = sprintf "\"%s\"" this.str

    static member generator = gen {
        //let! kind = Gen.choose(0, 1)
        let! num = Arb.generate<DecimalNumber>

        return Token (DecimalNumber num)
    }

    static member shrink (value : Token) = seq {
        match value.token with
        | DecimalNumber n ->
            yield!
                DecimalNumber.shrink n
                |> Seq.map (DecimalNumber >> Token)
    }

    static member arb = Arb.fromGenShrink (Token.generator, Token.shrink)

and private Token' =
    | DecimalNumber of DecimalNumber