namespace ParserCombinatorsFsharp.Tests

module SpacesTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData "";
      InlineData "  ";
      InlineData "  12312 ";>]
    let ``spaces should consume all the whitespace characters until fail`` input =
        input
        |> runOnString spaces
        |> assertSuccess ()
    
    [<Theory;
      InlineData ("", "");
      InlineData (" ", "");
      InlineData ("         ", "");
      InlineData ("     213123    ", "213123    ");>]
    let ``spaces should consume the input it parsed`` input expected =
        input
        |> runOnString spaces
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "  ";
      InlineData "  12312 ";>]
    let ``spaces1 should consume all the whitespace characters until fail`` input =
        input
        |> runOnString spaces1
        |> assertSuccess ()
    
    [<Theory;
      InlineData (" ", "");
      InlineData ("         ", "");
      InlineData ("     213123    ", "213123    ");>]
    let ``spaces1 should consume the input it parsed`` input expected =
        input
        |> runOnString spaces1
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "1 2312 ";>]
    let ``spaces1 should fail if the first char is not whitespace`` input =
        input
        |> runOnString spaces1
        |> assertFail