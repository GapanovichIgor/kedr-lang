namespace Kedr

[<Struct>]
type ParseSuccess<'a> =
    { value: 'a
      length: int }

type internal ParseResult<'a> = Result<ParseSuccess<'a>, unit> // TODO error

module internal ParseResult =
    let map (f: 'a -> 'b) (r: ParseResult<'a>): ParseResult<'b> =
        match r with
        | Ok ok ->
            { value = f ok.value
              length = ok.length }
            |> Ok
        | Error e -> Error e

type internal Parser<'i, 'o> = Tape<'i> -> ParseResult<'o>
