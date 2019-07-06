module private Kedr.Utils

open System

let (|Digit|_|) c =
    if Char.IsDigit c then Some c
    else None

let (|WhiteSpace|_|) c =
    if c = ' ' || c = '\t' then Some c
    else None

let inline getSubArray startInd length (collection: array<_>) =
    assert (startInd + length <= collection.Length)

    let result = Array.zeroCreate length

    for i = 0 to length - 1 do
        result.[i] <- collection.[i + startInd]

    result
