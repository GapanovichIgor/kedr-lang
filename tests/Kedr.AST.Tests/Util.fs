module Kedr.AST.Tests.Util
open FsCheck
open FsCheck.Xunit

let (==) x y = x = y |@ sprintf "%A = %A" x y

type PropertyOnceAttribute() =
    inherit PropertyAttribute(MaxTest = 1)