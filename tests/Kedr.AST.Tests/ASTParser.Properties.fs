[<FsCheck.Xunit.Properties>]
module Kedr.AST.Tests.ASTParser.Properties

open Kedr.AST
open Kedr.AST.Parser
open Kedr.AST.Tests.Util

type private T = Kedr.Tokenization.Token

let [<PropertyOnce>] ``expression of identifier reference is parsed as such``
    () =
    parse [ T.Identifier "i" ] == Ok (PExpr (IdRef "i"))

let [<PropertyOnce>] ``expression of string literal is parsed as such``
    () =
    parse [ T.QuotedString "asd" ] == Ok (PExpr (StrLit "asd"))

let [<PropertyOnce>] ``expression of func application is parsed as such``
    () =
    parse [ T.Identifier "foo"; T.QuotedString "hello" ] == Ok (PExpr (Application (IdRef "foo", StrLit "hello")))

let [<PropertyOnce>] ``func application is left associative``
    () =
    parse [ T.Identifier "foo"; T.Identifier "x"; T.Identifier "y" ] ==
        Ok (PExpr (Application (Application (IdRef "foo", IdRef "x"), IdRef "y")))

let [<PropertyOnce>] ``const binding is parser as such``
    () =
    parse [ T.Let; T.Identifier "foo"; T.Equals; T.QuotedString "hello" ] ==
        Ok (PBinding { name = "foo"; parameters = []; typeAnnotation = None; body = StrLit "hello" })

let [<PropertyOnce>] ``binding with type annotation is parser as such``
    () =
    parse [ T.Let; T.Identifier "foo"; T.Colon; T.Identifier "int"; T.Equals; T.QuotedString "hello" ] ==
        Ok (PBinding { name = "foo"; parameters = []; typeAnnotation = Some "int"; body = StrLit "hello" })

let [<PropertyOnce>] ``binding with parameters is parser as such``
    () =
    parse [
        T.Let
        T.Identifier "foo"
        T.ParenOpen
        T.Identifier "x"
        T.Colon
        T.Identifier "int"
        T.ParenClose
        T.Equals
        T.QuotedString "hello"
    ] ==
        Ok (
            PBinding {
                name = "foo"
                parameters = [{ name = "x"; typeAnnotation = Some "int" }]
                typeAnnotation = None
                body = StrLit "hello"
            }
        )