[<FsCheck.Xunit.Properties(Arbitrary = [| typeof<Kedr.AST.Tests.Arbs> |])>]
module Kedr.AST.Tests.ASTParser.Properties

open Kedr.AST
open Kedr.AST.Tests.Util

open FsCheck.Xunit
open Kedr.AST.Tests.Data.IdentifierToken

type private TK = Kedr.Tokenization.Token

let private parseExpr tokens = ASTParser.parseValueExpression tokens

let [<Property>] ``expression of identifier reference is parsed as such``
    (ident : string) =
    
    parseExpr [ TK.Identifier ident ] == Ok (IdentifierRef ident)
    
let [<Property>] ``application is parsed as such``
    (ident : string) =
    
    parseExpr [ TK.Identifier ident; TK.Identifier ident ] ==
        Ok (
           Application (
               IdentifierRef ident,
               IdentifierRef ident
           )
       )