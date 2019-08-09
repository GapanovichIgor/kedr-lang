module internal Kedr.Tokenization.Utils

let inline internal getSubArray startInd length (collection: array<_>) =
    assert (startInd + length <= collection.Length)

    let result = Array.zeroCreate length

    for i = 0 to length - 1 do
        result.[i] <- collection.[i + startInd]

    result
