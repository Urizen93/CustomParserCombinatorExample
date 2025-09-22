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
                            output.WriteLine <| parsed.Value.ToString ()
        | Fail fail -> let error = fail.ToString ()
                       Assert.Fail error

and Lambdas() as this =
    inherit TheoryData<string, LanguageExpression>()
    do
        this.Add("fun () -> 1", Lambda (Parameters <| create [UnitLiteral], [], Constant <| Literal.Integer 1))
        this.Add("fun x -> x", Lambda (Parameters <| create [Inferred <| Identifier.create "x"], [], Variable <| Identifier.create "x"))
        this.Add(
            "fun (x : string) f (i : int) -> i",
            Lambda (
                Parameters <| create [
                    Implicit <| (Identifier.create "x", String)
                    Inferred <| Identifier.create "f"
                    Implicit <| (Identifier.create "i", Integer)
                ],
                [],
                Variable <| Identifier.create "i"))
        this.Add(
            "fun (person : Customer) ->
                        let name = person.Name
                        name",
            Lambda (
                Parameters <| create [Implicit <| (Identifier.create "person", Custom <| Identifier.create "Customer")],
                [Statement (Assignment <| (Identifier.create "name", PropertyAccess (Identifier.create "person", Identifier.create "Name")))],
                Variable <| Identifier.create "name")
        )