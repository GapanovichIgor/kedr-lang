[<FsCheck.Xunit.Properties>]
module Kedr.AST.Tests.ASTParser.Properties

open Kedr.AST
open Kedr.AST.Tests.Util

type private TK = Kedr.Tokenization.Token

let private parse tokens = ASTParser.parse tokens

let [<PropertyOnce>] ``expression of identifier reference is parsed as such``
    () =
    parse [ TK.Identifier "i" ] == Ok (IdentifierRef "i")

let [<PropertyOnce>] ``expression of string literal is parsed as such``
    () =
    parse [ TK.QuotedString "asd" ] == Ok (StringLiteral "asd")

//let [<PropertyOnce>] ``expression of func application is parsed as such``
//    () =
//    parse [ TK.Identifier "foo"; TK.QuotedString "hello" ] ==
//        Ok (
//            InfixApplication (
//                Start (
//                    ArgumentFor (
//                        Function (IdentifierRef "foo"),
//                        StringLiteral "hello"
//                        )
//                    )
//                )
//            )
//
//let [<PropertyOnce>] ``func application is left associative``
//    () =
//    parse [ TK.Identifier "foo"; TK.Identifier "x"; TK.Identifier "y" ] ==
//        Ok (
//            PrefixApplication (
//                FunctionApplication (
//                    IdentifierRef "foo",
//                    IdentifierRef "x"
//                    ),
//                IdentifierRef "y"
//                )
//        )