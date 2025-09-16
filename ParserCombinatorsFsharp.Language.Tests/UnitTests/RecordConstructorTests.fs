namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Language.ProgramParser
open ParserCombinatorsFsharp.Tests

module RecordConstructorTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Fact>]
    let ``recordConstructor should parse a record`` () =
        " { First =();Second2 = 1    ;
            th_ird=\"test\"}"
        |> runOnString recordConstructor
        |> assertSuccess (RecordConstructor <| Map.ofList [(Identifier.create "First", Constant <| Literal.Unit)
                                                           (Identifier.create "Second2", Constant <| Literal.Integer 1)
                                                           (Identifier.create "th_ird", Constant <| Literal.String "test")])