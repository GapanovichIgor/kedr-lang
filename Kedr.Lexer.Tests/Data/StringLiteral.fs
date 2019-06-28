namespace Kedr.Lexer.Tests

open FsCheck

type StringLiteral(contents) =

    member val contents = contents
    member val str = "\"" + contents + "\""

    override this.ToString() = this.str

    static member generator = gen {
        let! contents =
            Arb.generate<string>

        let contents = contents |> String.filter (fun c -> c <> '"')

        return StringLiteral(contents)
    }

    static member shrink (value : StringLiteral) = seq {
        if value.contents.Length > 0 then
            yield StringLiteral(value.contents.Substring(1))
            yield StringLiteral(value.contents.Substring(0, value.contents.Length - 1))
    }

    static member arb = Arb.fromGenShrink (StringLiteral.generator, StringLiteral.shrink)