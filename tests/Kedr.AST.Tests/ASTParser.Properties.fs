[<FsCheck.Xunit.Properties>]
module Kedr.AST.Tests.ASTParser.Properties

open Kedr.AST
open Kedr.AST.Parser
open Kedr.AST.Tests.Util

type private T = Kedr.Tokenization.Token

let [<PropertyOnce>] ``expression of identifier reference is parsed as such``
    () =
    parse [ T.Identifier "i" ] == Ok (IdRef "i")

let [<PropertyOnce>] ``expression of string literal is parsed as such``
    () =
    parse [ T.QuotedString "asd" ] == Ok (StrLit "asd")

let [<PropertyOnce>] ``expression of func application is parsed as such``
    () =
    parse [ T.Identifier "foo"; T.QuotedString "hello" ] == Ok (Application (IdRef "foo", StrLit "hello"))

let [<PropertyOnce>] ``func application is left associative``
    () =
    parse [ T.Identifier "foo"; T.Identifier "x"; T.Identifier "y" ] ==
        Ok (Application (Application (IdRef "foo", IdRef "x"), IdRef "y"))