namespace Kedr.Lexer.Tests

open FsCheck

type TestAnyToken private (token) =
    member val private token = token

    member __.str =
        match token with
        | DecimalNumber n -> n.str
        | StringLiteral s -> s.str
        | Identifier i -> i.str
        | Let -> "let"
        | Type -> "type"
        | Plus -> "+"
        | Minus -> "-"
        | Asterisk -> "*"
        | Slash -> "/"
        | Equals -> "="
        | NotEquals -> "/="
        | ParenOpen -> "("
        | ParenClose -> ")"

    override this.ToString() = this.str

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
        | Identifier i ->
            yield!
                TestIdentifier.shrink i
                |> Seq.map (Identifier >> TestAnyToken)
        | Let
        | Type
        | Plus
        | Minus
        | Asterisk
        | Slash
        | Equals
        | ParenOpen
        | ParenClose
        | NotEquals -> ()
    }

    static member arb = Arb.fromGenShrink (TestAnyToken.generator, TestAnyToken.shrink)

and private Token' =
    | DecimalNumber of TestNumber
    | StringLiteral of TestQuotedString 
    | Identifier of TestIdentifier
    | Let
    | Type
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Equals
    | ParenOpen
    | ParenClose
    | NotEquals
