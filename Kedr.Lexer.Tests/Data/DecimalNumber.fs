namespace Kedr.Lexer.Tests

open FsCheck
open Kedr.Lexer.Tests

type DecimalNumber(isNegative, whole, fractional) =

    static let digits = [| for c in ['0'..'9'] -> c |]

    member val private isNegative = isNegative
    member val private whole = whole
    member val private fractional = fractional

    member val str =
        let sign = if isNegative then "-" else ""
        sign + whole + "." + fractional

    override this.ToString() = sprintf "\"%s\"" this.str

    static member generator = gen {
        let! isNegative = Gen.flipCoin()
        let! whole = Gen.stringOf digits 1 100
        let! fractional = Gen.stringOf digits 0 100

        return DecimalNumber(isNegative, whole, fractional)
    }

    static member shrink (num : DecimalNumber) = seq {
        if num.isNegative then
            yield DecimalNumber(false, num.whole, num.fractional)
        if num.whole.Length > 1 then
            yield DecimalNumber(num.isNegative, num.whole.Substring(1),                       num.fractional)
            yield DecimalNumber(num.isNegative, num.whole.Substring(0, num.whole.Length - 1), num.fractional)
        if num.fractional.Length > 1 then
            yield DecimalNumber(num.isNegative, num.whole,  num.fractional.Substring(1)                           )
            yield DecimalNumber(num.isNegative, num.whole,  num.fractional.Substring(0, num.fractional.Length - 1))
     }

    static member arb = Arb.fromGenShrink (DecimalNumber.generator, DecimalNumber.shrink)