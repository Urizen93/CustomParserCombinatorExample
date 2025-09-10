namespace ParserCombinatorsFsharp.Language

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Parser
open ParserCombinatorsFsharp.Helpers

module LanguageParser =
    [<Literal>]
    let IdentifierErrorMessage =
        "Identifier must consist of latin letters, digits, or underscores, and cannot be one of the reserved keywords!"
        
    let identifier =
        spaces >>. many1Char2 isLetter (isLetter <|> isDigit <|> %'_') .>> spaces
        |>> Identifier.createOrNone
        >>= fun maybeIdentifier -> lift <!> maybeIdentifier <|>% fail IdentifierErrorMessage
    
    let equality = %'='
    let doubleQuote = %'"'
    let curlyOpen = %'{'
    let curlyClose = %'}'
    let colon = %':'
    let semicolon = %';'
    let newLine = %'\n'
    let escapedDoubleQuote = %'\\' >>. doubleQuote
    
    let unitValue : Parser<unit> = exact "()"
    
    let typeKeyword : Parser<unit> = exact Keyword.Type
    let letKeyword : Parser<unit> = exact Keyword.Let
    let unitKeyword : Parser<unit> = exact Keyword.Unit
    let intKeyword : Parser<unit> = exact Keyword.Int
    let stringKeyword : Parser<unit> = exact Keyword.String
    
    let intLiteral : Parser<Literal> = many1Char isDigit |>> (int >> Literal.Integer)
    let stringLiteral : Parser<Literal> =
        (escapedDoubleQuote <|> anyChar)
        |> manyCharBetween2 doubleQuote
        |>> Literal.String
    
    let compileTimeType : Parser<CompileTimeType> =
        spaces >>.
        unitKeyword |>> fun _ -> Unit
        <|> (intKeyword |>> fun _ -> Integer)
        <|> (stringKeyword |>> fun _ -> String)
        <|> (identifier |>> Custom)
        .>> spaces
    
    let typeName = spaces >>. typeKeyword >>. spaces1 >>. identifier .>> spaces .>> equality .>> spaces
    let recordField =
        identifier .>>. colon .>>. compileTimeType
        |>> fun ((fieldName, _), fieldType) -> (fieldName, fieldType)
    let recordFields =
        let fieldSeparator = (spaces >>. newLine >>. spaces) <|> (spaces >>. semicolon >>. spaces)
        spaces >>. curlyOpen >>. spaces
        >>. (recordField |> sepBy fieldSeparator)
        .>> spaces .>> curlyClose .>> spaces
        >>= fun fields ->
                let result = Map.ofList fields
                if result.Count = fields.Length
                then lift result
                else fail "Duplicate record fields are not allowed"
    let recordDefinition : Parser<RecordDefinition> = typeName .>>. recordFields