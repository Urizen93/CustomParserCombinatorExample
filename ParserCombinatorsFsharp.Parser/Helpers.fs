namespace ParserCombinatorsFsharp

module Helpers =
    open Microsoft.FSharp.Core

    let (<!>) = Option.map

    let (<|>%) option resolution = Option.defaultValue resolution option