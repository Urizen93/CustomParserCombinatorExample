namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Language.ProgramParser
open Xunit
open ParserCombinatorsFsharp.Parser
open NonEmptyList

type ``lambda parser tests`` (output : ITestOutputHelper) =
    
    [<Theory; ClassData(typeof<Lambdas>)>]
    member _.``Should parse a lambda`` (input, expected : LanguageExpression)=
        match input |> runOnString lambda with
        | Success parsed -> Assert.Equal (expected, parsed.Value)
        | Fail (Error fail) -> output.WriteLine fail

and Lambdas() as this =
    inherit TheoryData<string, LanguageExpression>()
    do
        this.Add("fun () -> 1", Lambda (None, [], Constant <| Literal.Integer 1))
        this.Add("fun x -> x", Lambda (Parameters <| create [Inferred <| Identifier.create "x"], [], Variable <| Identifier.create "x"))
        this.Add(
            "fun (person : Customer) ->
                        let name = person.Name
                        name",
            Lambda (
                Parameters <| create [Implicit <| (Identifier.create "person", Custom <| Identifier.create "Customer")],
                [Statement (Assignment <| (Identifier.create "name", PropertyAccess (Identifier.create "person", Identifier.create "name")))],
                Variable <| Identifier.create "name")
        )