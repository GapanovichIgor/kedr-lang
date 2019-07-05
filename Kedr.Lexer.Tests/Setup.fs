module Kedr.Lexer.Tests.Setup

open Expecto

type Arbs =
    static member Whitespace () = TestWhitespace.arb
    static member Number () = TestNumber.arb
    static member QuotedString () = TestQuotedString.arb
    static member AnyToken () = TestAnyToken.arb

let inline (++) res1 res2 =
    match res1, res2 with
    | Ok tokens1, Ok tokens2 -> Ok (tokens1 @ tokens2)
    | er, Ok _
    | Ok _, er -> er
    | Error reason1, Error reason2 -> Error (reason1 + "|" + reason2)

let inline toStr x =
    ( ^a : (member str : string with get) x )
    
let inline (!) x = toStr x

let cfg = {
    FsCheckConfig.defaultConfig with
        arbitrary = typeof<Arbs> :: FsCheckConfig.defaultConfig.arbitrary
}

let inline (:=) propName propTest = testPropertyWithConfig cfg propName propTest