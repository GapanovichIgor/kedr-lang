namespace Kedr.Tokenization

type internal IndentationStyle =
    | Space
    | Tab
    | Mixed

[<Struct>]
type internal TokenizerState =
    { indentationLevels : int list
      indentationStyle : IndentationStyle option }

module internal TokenizerState =
    let initial =
        { indentationLevels = []
          indentationStyle = None }
