namespace ParserCombinatorsFsharp.Tests

open Microsoft.FSharp.Core
open Xunit
open ParserCombinatorsFsharp

[<AutoOpen>]
module Assertions =
    let assertSuccess expected actual =
        match actual with
        | Success { Value = actualValue } when actualValue = expected -> ()
        | Success { Value = actualValue } -> Assert.Fail $"Expected {expected}, actual {actualValue}"
        | Fail error -> Assert.Fail $"Parser failed with {error}"

    let assertRemainingInput expected actual =
        match actual with
        | Success { RemainingInput = actualRemainingInput } when expected = actualRemainingInput.ReadNext expected.Length -> ()
        | Success { RemainingInput = actualRemainingInput } -> Assert.Fail $"Expected to have {expected} remaining, actually got {actualRemainingInput.Sample}"
        | Fail error -> Assert.Fail $"Parser failed with {error}"

    let assertFail actual =
        match actual with
        | Fail _ -> ()
        | Success success -> Assert.Fail $"Parser was expected to fail but actually parsed {success}"