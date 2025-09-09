namespace ParserCombinatorsFsharp.Tests

module ManyTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("", "");
      InlineData ("123", "123");
      InlineData ("654a3", "654");>]
    let ``many should parse until fails`` input (expected : string) =
        input
        |> runOnString (many isDigit)
        |> assertSuccess (expected.ToCharArray() |> List.ofArray)
    
    [<Theory;
      InlineData ("", "");
      InlineData ("123", "");
      InlineData ("654a3", "a3");>]
    let ``many should consume the input it parsed`` input expected =
        input
        |> runOnString (many isDigit)
        |> assertRemainingInput expected
    
    [<Fact>]
    let ``many should short-circuit on non-consuming parsers`` () =
        "123456789"
        |> runOnString (many nonConsumingParser)
        |> assertFail
    
    [<Theory;
      InlineData ("123", "123");
      InlineData ("654a3", "654");>]
    let ``many1 should parse until fails`` input (expected : string) =
        input
        |> runOnString (many1 isDigit)
        |> assertSuccess (expected.ToCharArray() |> List.ofArray)
    
    [<Theory;
      InlineData ("123", "");
      InlineData ("654a3", "a3");>]
    let ``many1 should consume the input it parsed`` input expected =
        input
        |> runOnString (many1 isDigit)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "a654a3";>]
    let ``many1 should fail if it couldn't consume at least one symbol`` input =
        input
        |> runOnString (many1 isDigit)
        |> assertFail
    
    [<Fact>]
    let ``many1 should short-circuit on non-consuming parsers`` () =
        "123456789"
        |> runOnString (many1 nonConsumingParser)
        |> assertFail