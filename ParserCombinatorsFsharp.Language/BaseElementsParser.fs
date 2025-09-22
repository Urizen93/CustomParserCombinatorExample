namespace ParserCombinatorsFsharp.Language

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Parser
open ParserCombinatorsFsharp.Helpers

module LanguageParser =
    // shamelessly stolen directly from FParsec
    let createParserForwardedToRef () =
        let dummyParser = fun _ -> failwith "a parser created with createParserForwardedToRef was not initialized"
        let reference = ref dummyParser
        (fun input -> reference.Value input), reference : Parser<_> * Parser<_> ref
    
    [<Literal>]
    let IdentifierErrorMessage =
        "Identifier must consist of latin letters, digits, or underscores, and cannot be one of the reserved keywords!"
        
    let identifier : Parser<Identifier> =
        spaces >>. many1Char2 isLetter (isLetter <|> isDigit <|> %'_') .>> lineSpaces
        |>> Identifier.createOrNone
        >>= fun maybeIdentifier -> lift <!> maybeIdentifier <|>% fail IdentifierErrorMessage
    
    let dot : Parser<char> = %'.'
    let equality : Parser<char> = %'='
    let doubleQuote : Parser<char> = %'"'
    let curlyOpen : Parser<char> = %'{'
    let curlyClose : Parser<char> = %'}'
    let bracketOpen : Parser<char> = %'['
    let bracketClose : Parser<char> = %']'
    let parenOpen : Parser<char> = %'('
    let parenClose : Parser<char> = %')'
    let colon : Parser<char> = %':'
    let semicolon : Parser<char> = %';'
    let escapedDoubleQuote = %'\\' >>. doubleQuote
    let arrow : Parser<unit> = exact "->"
    
    let typeKeyword : Parser<unit> = exact Keyword.Type
    let letKeyword : Parser<unit> = exact Keyword.Let
    let funKeyword : Parser<unit> = exact Keyword.Fun
    let returnKeyword : Parser<unit> = exact Keyword.Return
    let unitKeyword : Parser<unit> = exact Keyword.Unit
    let intKeyword : Parser<unit> = exact Keyword.Int
    let stringKeyword : Parser<unit> = exact Keyword.String
    
    let unitLiteral: Parser<Literal> = exact "()" |>> fun _ -> Literal.Unit
    let intLiteral : Parser<Literal> = many1Char isDigit |>> (int >> Literal.Integer)
    let stringLiteral : Parser<Literal> =
        (escapedDoubleQuote <|> anyChar)
        |> manyCharBetween2 doubleQuote
        |>> Literal.String