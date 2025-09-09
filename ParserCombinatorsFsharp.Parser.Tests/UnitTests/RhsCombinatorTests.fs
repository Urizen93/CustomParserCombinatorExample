namespace ParserCombinatorsFsharp.Tests

module RhsCombinatorTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("a2", "2");
      InlineData ("q5632", "5")>]
    let ``>>. should return rhs result`` input expected =
        input
        |> runOnString (isLetter >>. isDigit)
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("a2", "");
      InlineData ("q5632", "632")>]
    let ``>>. should consume as rhs parser`` input expected =
        input
        |> runOnString (isLetter >>. isDigit)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "1";
      InlineData "12wqe2">]
    let ``>>. should fail if lhs parser failed`` input =
        input
        |> runOnString (isLetter >>. isDigit)
        |> assertFail
    
    [<Theory;
      InlineData "x";
      InlineData "bwqe2">]
    let ``>>. should fail if rhs parser failed`` input =
        input
        |> runOnString (isLetter >>. isDigit)
        |> assertFail