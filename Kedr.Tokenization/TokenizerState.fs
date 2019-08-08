namespace Kedr.Tokenization

type IndentationStyle =
    | Space
    | Tab
    | Mixed

[<Struct>]
type TokenizerState =
    { indentationLevels : int list
      indentationStyle : IndentationStyle option }

module TokenizerState =
    let initial =
        { indentationLevels = []
          indentationStyle = None }
