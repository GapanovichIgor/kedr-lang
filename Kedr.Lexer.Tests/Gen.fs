module Kedr.Lexer.Tests.Gen
open System.Text
open FsCheck

let flipCoin () = gen {
    let! coin = Gen.choose(0, 1)
    return
        match coin with
        | 0 -> false
        | _ -> true
}

let stringOf (chars : char array) lowerBound upperBound = gen {
    let builder = StringBuilder()
    let! length = Gen.choose(lowerBound, upperBound)
    let charsLastIndex = (chars |> Array.length) - 1
    for _ in [0..length] do
        let! charInd = Gen.choose(0, charsLastIndex)
        builder.Append(chars.[charInd]) |> ignore

    return builder.ToString()
}