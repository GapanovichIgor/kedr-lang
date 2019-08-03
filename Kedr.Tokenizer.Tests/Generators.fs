module Kedr.Tokenizer.Tests.Generators

open Hedgehog

let numberGen = gen {
    let uint32Gen = Range.exponentialBounded () |> Gen.uint32
    let! integerPart = uint32Gen 
    
    let! hasFractionalPart = Gen.bool
    
    if hasFractionalPart then
        let! fractionalPart = uint32Gen
        let text = sprintf "%u.%u" integerPart fractionalPart
        return (text, integerPart, Some fractionalPart)
    else
        let text = integerPart.ToString()
        return (text, integerPart, None)
}

let quotedStringGen = gen {
    let! content =
        Gen.unicode
        |> Gen.filter (fun c -> c <> '"')
        |> Gen.string (Range.exponential 0 1000)
        
    let text = sprintf "\"%s\"" content
        
    return (text, content)
}

let identifierGen = gen {
    let! firstChar =
        Gen.choice [
            Gen.alpha
            Gen.constant '_'
        ]
        
    let followingCharGen =
        Gen.choice [
            Gen.alpha
            Gen.constant '_'
            // TODO остальные символы
        ]
        
    let! partAfterFirstChar =
        followingCharGen
        |> Gen.string (Range.exponential 0 10000)
        
    return sprintf "%c%s" firstChar partAfterFirstChar
}

let anyTokenGen =
    Gen.choice [
        identifierGen
        quotedStringGen |> Gen.map (fun (text, _) -> text)
        numberGen |> Gen.map (fun (text, _, _) -> text)
        Gen.constant "let"
        Gen.constant "type"
        Gen.constant "+"
        Gen.constant "-"
        Gen.constant "*"
        Gen.constant "/"
        Gen.constant "="
        Gen.constant "/="
        Gen.constant "("
        Gen.constant ")"
    ]

let whitespaceGen =
    Gen.item [ ' '; '\t' ]
    |> Gen.string (Range.constant 1 10)