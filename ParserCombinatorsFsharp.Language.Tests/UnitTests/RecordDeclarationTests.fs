namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Tests

module RecordDefinitionTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    open ParserCombinatorsFsharp.Language.LanguageParser
    
    [<Fact>]
    let ``recordDefinition should parse record`` () =
        " type Test={First:unit;Second2:int    ;  th_ird:string
        A:
        unit}"
        |> runOnString recordDefinition
        |> assertSuccess (Identifier.create "Test", Map.ofList [(Identifier.create "First", Unit)
                                                                (Identifier.create "Second2", Integer)
                                                                (Identifier.create "th_ird", String)
                                                                (Identifier.create "A", Unit)])
    [<Fact>]
    let ``recordDefinition should allow empty records`` () =
        "type Empty = {}"
        |> runOnString recordDefinition
        |> assertSuccess (Identifier.create "Empty", Map.empty)