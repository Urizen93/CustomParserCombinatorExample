namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Language.ProgramParser
open ParserCombinatorsFsharp.Tests

module RecordDefinitionTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Fact>]
    let ``recordDefinition should parse a record`` () =
        " type Test={First: unit;Second2 :int    ;  th_1rd:string
        A:
        CustomType}"
        |> runOnString recordDefinition
        |> assertSuccess (RecordDefinition >> TypeDefinition
                          <| (Identifier.create "Test", Map.ofList [(Identifier.create "First", Unit)
                                                                    (Identifier.create "Second2", Integer)
                                                                    (Identifier.create "th_1rd", String)
                                                                    (Identifier.create "A", Custom <| Identifier.create "CustomType")]))