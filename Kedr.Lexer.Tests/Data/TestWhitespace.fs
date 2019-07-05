namespace Kedr.Lexer.Tests

open FsCheck
open System.Text

type TestWhitespace(str) =
    member val str = str

    override __.ToString() =
        let prettyStr = str |> String.collect (fun c ->
            match c with
            | '\t' -> "\\t"
            | ' ' -> "\\s"
            | c -> string c)

        sprintf "\"%s\"" prettyStr

    static member val generator = gen {
        let builder = StringBuilder()

        let! length = Gen.choose(1, 10)

        for _ in [0..length] do
            let! kind = Gen.choose(0, 1)

            let char =
                match kind with
                | 0 -> ' '
                | _ -> '\t'

            builder.Append(char) |> ignore

        return TestWhitespace (builder.ToString())
    }

    static member shrink (value : TestWhitespace) = seq {
        if value.str.Length > 1 then
            yield value.str.Substring(1) |> TestWhitespace
            yield value.str.Substring(0, value.str.Length - 1) |> TestWhitespace
    }

    static member arb = Arb.fromGenShrink (TestWhitespace.generator, TestWhitespace.shrink)