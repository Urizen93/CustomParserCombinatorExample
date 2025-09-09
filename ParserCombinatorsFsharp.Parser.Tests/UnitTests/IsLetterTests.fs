namespace ParserCombinatorsFsharp.Tests

module IsLetterTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("a", "a");
      InlineData ("bar", "b")>]
    let ``isLetter should return letter`` input expected =
        input
        |> runOnString isLetter
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("a", "");
      InlineData ("bar", "ar")>]
    let ``isLetter should consume one char`` input expected =
        input
        |> runOnString isLetter
        |> assertRemainingInput expected

    [<Theory;
      InlineData "";
      InlineData "4";
      InlineData "$sdfg";
      InlineData "_sdfg">]
    let ``isLetter should fail on non-letters`` input =
        input
        |> runOnString isLetter
        |> assertFail