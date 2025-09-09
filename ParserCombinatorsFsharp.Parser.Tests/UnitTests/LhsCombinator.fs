namespace ParserCombinatorsFsharp.Tests

module LhsCombinatorTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("a2", "a");
      InlineData ("q5632", "q")>]
    let ``.>> should return lhs result`` input expected =
        input
        |> runOnString (isLetter .>> isDigit)
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("a2", "");
      InlineData ("q5632", "632")>]
    let ``.>> should consume as rhs parser`` input expected =
        input
        |> runOnString (isLetter .>> isDigit)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "1";
      InlineData "12wqe2">]
    let ``.>> should fail if lhs parser failed`` input =
        input
        |> runOnString (isLetter .>> isDigit)
        |> assertFail
    
    [<Theory;
      InlineData "x";
      InlineData "bwqe2">]
    let ``.>> should fail if rhs parser failed`` input =
        input
        |> runOnString (isLetter .>> isDigit)
        |> assertFail