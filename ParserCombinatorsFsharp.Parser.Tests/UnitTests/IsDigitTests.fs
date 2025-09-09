namespace ParserCombinatorsFsharp.Tests

module IsDigitTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
    InlineData ("1", "1");
    InlineData ("5632", "5")>]
    let ``isDigit should return digit`` input expected =
        input
        |> runOnString isDigit
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("1", "");
      InlineData ("5632", "632")>]
    let ``isDigit should consume one char`` input expected =
        input
        |> runOnString isDigit
        |> assertRemainingInput expected

    [<Theory;
      InlineData "";
      InlineData "a";
      InlineData "q5632">]
    let ``isDigit should fail on non-digits`` input =
        input
        |> runOnString isDigit
        |> assertFail