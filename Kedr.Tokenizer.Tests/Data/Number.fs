namespace Kedr.Tokenizer.Tests.Data
open FsCheck

type Number(integerPart, fractionalPart) =
    member val integerPart = integerPart
    member val fractionalPart = fractionalPart

    member val tokenText =
        match fractionalPart with
        | Some f -> sprintf "%u.%u" integerPart f
        | None -> sprintf "%u" integerPart

    override this.ToString() = this.tokenText

module Number =

    let (|Number''|) (n : Number) =
        (n.tokenText, n.integerPart, n.fractionalPart)

    let (|Number'|) (n : Number) =
        n.tokenText

    let gen =
        Gen.zip
            Arb.generate<uint32>
            (Arb.generate<uint32> |> Gen.optionOf)
        |> Gen.map Number

    let shrink (n : Number) =
        seq {
            match n.fractionalPart with
            | Some fractionalPart ->
                yield Number(n.integerPart, None)

                yield!
                    Arb.shrink fractionalPart
                    |> Seq.map (fun f -> Number(n.integerPart, Some f))

                yield!
                    Arb.shrink n.integerPart
                    |> Seq.map (fun i -> Number(i, Some fractionalPart))
            | None ->
                yield!
                    Arb.shrink n.integerPart
                    |> Seq.map (fun i -> Number(i, None))
        }

    let arb = Arb.fromGenShrink (gen, shrink)
