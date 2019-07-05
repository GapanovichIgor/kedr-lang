namespace Kedr.Lexer.Tests

open FsCheck

type TestQuotedString(contents) =

    member val contents = contents
    member val str = "\"" + contents + "\""

    override this.ToString() = this.str

    static member generator = gen {
        let! contents = Arb.generate<string>

        let contents = contents |> String.filter (fun c -> c <> '"')

        return TestQuotedString(contents)
    }

    static member shrink (value : TestQuotedString) = seq {
        if value.contents.Length > 0 then
            yield TestQuotedString(value.contents.Substring(1))
            yield TestQuotedString(value.contents.Substring(0, value.contents.Length - 1))
    }

    static member arb = Arb.fromGenShrink (TestQuotedString.generator, TestQuotedString.shrink)