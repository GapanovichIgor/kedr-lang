namespace Kedr.Lexer.Tests

open FsCheck
open Kedr.Lexer.Tests

type TestNumber(integerPart : int64, fractionalPart : int64 option) =

    member val integerPart = integerPart
    member val fractionalPart = fractionalPart

    member val str =
        match fractionalPart with
        | Some fractionalPart -> (string integerPart) + "." + (string fractionalPart)
        | None -> string integerPart

    override this.ToString() = sprintf "\"%s\"" this.str

    static member generator = gen {
        let! integerPart = Arb.generate<uint32>
        
        let! hasFractionalPart = Gen.flipCoin()
        
        if hasFractionalPart then
            let! fractionalPart = Arb.generate<uint32>
            return TestNumber(int64 integerPart, Some (int64 fractionalPart))
        else
            return TestNumber(int64 integerPart, None)
    }

    static member shrink (num : TestNumber) = seq {
        match num.fractionalPart with
        | Some fractionalPart ->
            if fractionalPart > 9L then
                let fractionalPart = fractionalPart / 10L
                yield TestNumber(num.integerPart, Some fractionalPart)
            else
                yield TestNumber(num.integerPart, None)
        | None -> ()
        
        if num.integerPart > 9L then
            let integerPart = num.integerPart / 10L
            yield TestNumber(integerPart, num.fractionalPart)
     }

    static member arb = Arb.fromGenShrink (TestNumber.generator, TestNumber.shrink)