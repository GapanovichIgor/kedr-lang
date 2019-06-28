﻿namespace Kedr.Lexer.Tests

open FsCheck
open Kedr.Lexer.Tests

type DecimalNumberLiteral(isNegative, whole, fractional) =

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

        return DecimalNumberLiteral(isNegative, whole, fractional)
    }

    static member shrink (num : DecimalNumberLiteral) = seq {
        if num.isNegative then
            yield DecimalNumberLiteral(false, num.whole, num.fractional)
        if num.whole.Length > 1 then
            yield DecimalNumberLiteral(num.isNegative, num.whole.Substring(1),                       num.fractional)
            yield DecimalNumberLiteral(num.isNegative, num.whole.Substring(0, num.whole.Length - 1), num.fractional)
        if num.fractional.Length > 1 then
            yield DecimalNumberLiteral(num.isNegative, num.whole,  num.fractional.Substring(1)                           )
            yield DecimalNumberLiteral(num.isNegative, num.whole,  num.fractional.Substring(0, num.fractional.Length - 1))
     }

    static member arb = Arb.fromGenShrink (DecimalNumberLiteral.generator, DecimalNumberLiteral.shrink)