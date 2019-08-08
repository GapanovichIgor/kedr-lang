namespace Kedr.Tokenization

[<Struct>]
type ParseSuccess<'v, 's> =
    { value: 'v
      state: 's
      length: int }

type internal ParseResult<'v, 's> = Result<ParseSuccess<'v, 's>, unit> // TODO error

module internal ParseResult =
    let mapValue (f: 'a -> 'b) (r: ParseResult<'a, _>): ParseResult<'b, _> =
        match r with
        | Ok ok ->
            { value = f ok.value
              state = ok.state
              length = ok.length }
            |> Ok
        | Error e -> Error e

type internal Parser<'i, 's, 'o> = Tape<'i> * 's -> ParseResult<'o, 's>
