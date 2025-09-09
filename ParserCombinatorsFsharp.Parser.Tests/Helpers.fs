namespace ParserCombinatorsFsharp.Tests

open ParserCombinatorsFsharp

[<AutoOpen>]
module Helpers =
    let nonConsumingParser = fun (input : Input) -> Success { Value = input.Rest; RemainingInput = input }