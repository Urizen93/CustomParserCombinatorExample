namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Tests

module StringLiteralTests =
    open Xunit
    open ParserCombinatorsFsharp.Parser
    open ParserCombinatorsFsharp.Language.LanguageParser
    
    [<Theory;
      InlineData ("\"\"", "");
      InlineData ("\"12a$3\"", "12a$3");
      InlineData ("\"12a\\\"$3\"", "12a\"$3");>]
    let ``stringLiteral should parse quoted string`` input expected =
        input
        |> runOnString stringLiteral
        |> assertSuccess (Literal.String expected)
    
    [<Theory;
      InlineData ("\"\"", "");
      InlineData ("\"12a$3\"asf sad", "asf sad");>]
    let ``stringLiteral should consume the input it parsed`` input expected =
        input
        |> runOnString stringLiteral
        |> assertRemainingInput expected
    
    [<Theory;
      InlineData "";
      InlineData "\"";
      InlineData "123\"12a$3\"";
      InlineData "\"12a\\\"$3";>]
    let ``stringLiteral should fail if input is not enclosed in quotes`` input =
        input
        |> runOnString stringLiteral
        |> assertFail