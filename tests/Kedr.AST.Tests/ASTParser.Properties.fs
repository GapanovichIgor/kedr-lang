[<FsCheck.Xunit.Properties>]
module Kedr.AST.Tests.ASTParser.Properties

open Kedr.AST
open Kedr.AST.Tests.Util

type private TK = Kedr.Tokenization.Token

let private parseExpr tokens = ASTParser.parseValueExpression tokens

let [<PropertyOnce>] ``expression of identifier reference is parsed as such``
    () =
    
    parseExpr [ TK.Identifier "i" ] == Ok (IdentifierRef "i")