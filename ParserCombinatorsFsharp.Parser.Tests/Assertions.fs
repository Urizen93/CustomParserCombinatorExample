namespace ParserCombinatorsFsharp.Tests

open Microsoft.FSharp.Core
open Xunit
open ParserCombinatorsFsharp

[<AutoOpen>]
module Assertions =
    let assertSuccess expected actual =
        match actual with
        | Success { Value = actualValue } when actualValue = expected -> ()
        | Success { Value = actualValue } -> failwith $"Expected {expected}, actual {actualValue}"
        | Fail (Error error) -> Assert.Fail $"Parser failed with {error}"

    let assertRemainingInput expected actual =
        match actual with
        | Success { RemainingInput = actualRemainingInput } when actualRemainingInput.Rest = expected -> ()
        | Success { RemainingInput = actualRemainingInput } -> failwith $"Expected to have {expected} remaining, actually got {actualRemainingInput.Rest}"
        | Fail (Error error) -> Assert.Fail $"Parser failed with {error}"

    let assertFail actual =
        match actual with
        | Fail _ -> ()
        | Success success -> Assert.Fail $"Parser was expected to fail but actually parsed {success}"