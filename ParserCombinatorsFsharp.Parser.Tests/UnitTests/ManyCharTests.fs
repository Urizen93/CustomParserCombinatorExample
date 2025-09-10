namespace ParserCombinatorsFsharp.Tests

module ManyCharTests =
    open FsUnit
    open Xunit
    open ParserCombinatorsFsharp.Parser
    
    [<Theory;
      InlineData ("123", "123");
      InlineData ("654a3", "654");>]
    let ``many1Char should parse until fails`` input (expected : string) =
        input
        |> runOnString (many1Char isDigit)
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("123", "");
      InlineData ("654a3", "a3");>]
    let ``many1Char should consume the input it parsed`` input expected =
        input
        |> runOnString (many1Char isDigit)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "a654a3";>]
    let ``many1Char should fail if it couldn't consume at least one symbol`` input =
        input
        |> runOnString (many1Char isDigit)
        |> assertFail
    
    [<Fact>]
    let ``many1Char should short-circuit on non-consuming parsers`` () =
        fun () -> "123456789" |> runOnString (many1Char (nonConsumingParser |>> fun _ -> '1')) |> ignore
        |> should throw typeof<System.Exception>
    
    [<Theory;
      InlineData ("x", "x");
      InlineData ("a123", "a123");
      InlineData ("w654a3", "w654");>]
    let ``many1Char2 should parse until fails`` input (expected : string) =
        input
        |> runOnString (many1Char2 isLetter isDigit)
        |> assertSuccess expected
    
    [<Theory;
      InlineData ("e123", "");
      InlineData ("r654a3", "a3");>]
    let ``many1Char2 should consume the input it parsed`` input expected =
        input
        |> runOnString (many1Char2 isLetter isDigit)
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "6w54a3";>]
    let ``many1Char2 should fail if it couldn't consume at least one symbol`` input =
        input
        |> runOnString (many1Char2 isLetter isDigit)
        |> assertFail
    
    [<Fact>]
    let ``many1Char2 should short-circuit if p2 is a non-consuming parser`` () =
        fun () -> "123456789" |> runOnString (many1Char2 isDigit (nonConsumingParser |>> fun _ -> '1')) |> ignore
        |> should throw typeof<System.Exception>
    
    [<Fact>]
    let ``many1Char2 should behave correctly if p1 is a non-consuming parser`` () =
        let input = "123456789"
        let ch = '1'
        input
        |> runOnString (many1Char2 (nonConsumingParser |>> fun _ -> ch) isDigit)
        |> assertSuccess ($"{ch}" + input)