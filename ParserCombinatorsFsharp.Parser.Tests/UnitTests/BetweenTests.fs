namespace ParserCombinatorsFsharp.Tests

module BetweenTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("{123}", "123");>]
    let ``between should return the results of the last parser`` input expected =
        input
        |> runOnString (between (exact "{") (exact "}") (many1Char isDigit))
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("{123}", "");
      InlineData ("{123}1qwe", "1qwe");>]
    let ``between should consume the input it parsed`` input expected =
        input
        |> runOnString (between (exact "{") (exact "}") (many1Char isDigit))
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "}123}";
      InlineData "{123{";
      InlineData "{}";>]
    let ``between should fail if any of the parsers fail`` input =
        input
        |> runOnString (between (exact "{") (exact "}") (many1Char isDigit))
        |> assertFail