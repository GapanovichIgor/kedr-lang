namespace Kedr.Lexer.Tests

open FsCheck

type TestAnyToken private (token) =
    member val private token = token

    member __.str =
        match token with
        | DecimalNumber n -> n.str
        | StringLiteral s -> s.str
        | Plus -> "+"

    override this.ToString() =
        match token with
        | DecimalNumber n -> n.ToString()
        | StringLiteral s -> s.ToString()
        | Plus -> "+"

    static member generator = gen {
        let! token = Arb.generate<Token'>

        return TestAnyToken(token)
    }

    static member shrink (value : TestAnyToken) = seq {
        match value.token with
        | DecimalNumber n ->
            yield!
                TestNumber.shrink n
                |> Seq.map (DecimalNumber >> TestAnyToken)
        | StringLiteral s ->
            yield!
                TestQuotedString.shrink s
                |> Seq.map (StringLiteral >> TestAnyToken)
        | Plus -> () 
    }

    static member arb = Arb.fromGenShrink (TestAnyToken.generator, TestAnyToken.shrink)

and private Token' =
    | DecimalNumber of TestNumber
    | StringLiteral of TestQuotedString
    | Plus