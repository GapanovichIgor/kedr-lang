namespace Kedr.Tokenization.Tests
open Kedr.Tokenization.Tests.Data

type Arbs =
    static member whitespace () = Whitespace.arb
    static member indentation () = Indentation.arb
    static member newLine () = NewLine.arb
    static member number' () = Number.arb
    static member number () = Number.arb
    static member quotedString' () = QuotedString.arb
    static member quotedString () = QuotedString.arb
    static member identifier () = Identifier.arb
    static member anyToken () = AnyToken.arb