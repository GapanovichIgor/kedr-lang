namespace Kedr.Parsing

[<Struct>]
type ParseSuccess<'value, 'state> =
    { value: 'value
      state: 'state
      length: int }

type ParseResult<'value, 'state, 'error> = Result<ParseSuccess<'value, 'state>, 'error>

module ParseResult =
    let mapValue (f: 'a -> 'b) (r: ParseResult<'a, _, _>): ParseResult<'b, _, _> =
        match r with
        | Ok ok ->
            { value = f ok.value
              state = ok.state
              length = ok.length }
            |> Ok
        | Error e -> Error e
        
    let mapError (f :'a -> 'b) (r: ParseResult<_, _, 'a>): ParseResult<_, _, 'b> =
        match r with
        | Ok ok -> Ok ok
        | Error e -> Error (f e)