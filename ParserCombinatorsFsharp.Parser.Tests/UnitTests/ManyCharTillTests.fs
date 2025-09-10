namespace ParserCombinatorsFsharp.Tests

module ManyCharTillTests =
    open FsUnit
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("123", "12", '3');
      InlineData ("654a3", "654", 'a');>]
    let ``manyCharTill should parse until fails`` input (expected : string) ending =
        input
        |> runOnString (manyCharTill isDigit %ending)
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("123", "", '3');
      InlineData ("654a3", "3", 'a');>]
    let ``manyCharTill should consume the input it parsed`` input expected ending =
        input
        |> runOnString (manyCharTill isDigit %ending)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData ("", "$");
      InlineData ("a654a3", "$");>]
    let ``manyCharTill should fail if it couldn't find an ending`` input ending =
        input
        |> runOnString (manyCharTill isDigit %ending)
        |> assertFail
    
    [<Fact>]
    let ``manyCharTill should short-circuit on non-consuming parsers`` () =
        fun () -> "123456789" |> runOnString (manyCharTill (nonConsumingParser |>> fun _ -> '1') %'9') |> ignore
        |> should throw typeof<System.Exception>