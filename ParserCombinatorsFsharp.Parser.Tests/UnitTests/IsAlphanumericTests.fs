namespace ParserCombinatorsFsharp.Tests

module IsAlphanumericTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("a", "a");
      InlineData ("7bar", "7")>]
    let ``isAlphanumeric should return letter or digit`` input expected =
        input
        |> runOnString isAlphanumeric
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("a", "");
      InlineData ("7bar", "bar")>]
    let ``isAlphanumeric should consume one char`` input expected =
        input
        |> runOnString isAlphanumeric
        |> assertRemainingInput expected

    [<Theory;
      InlineData "";
      InlineData ".";
      InlineData "$sdfg";
      InlineData "_sdfg">]
    let ``isAlphanumeric should fail on non-letters`` input =
        input
        |> runOnString isAlphanumeric
        |> assertFail