namespace ParserCombinatorsFsharp.Language

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Parser

module ProgramParser =
    open LanguageParser
    open NonEmptyList
    
    let compileTimeType : Parser<CompileTimeType> =
        spaces >>.
        choice [
            unitKeyword |>> fun _ -> Unit
            (intKeyword |>> fun _ -> Integer)
            (stringKeyword |>> fun _ -> String)
            (identifier |>> Custom)
        ]
        .>> spaces
    
    let typeName = spaces >>. typeKeyword >>. spaces1 >>. identifier .>> spaces .>> equality .>> spaces
    let recordFieldDefinition = identifier .>> colon .>>. compileTimeType
    let fieldSeparator = (endOfLine >>. spaces) <|> (spaces >>. semicolon >>. spaces)
    
    let recordFieldsDefinition =
        spaces >>. curlyOpen >>. spaces
        >>. (recordFieldDefinition |> sepBy fieldSeparator)
        .>> spaces .>> curlyClose
        >>= fun fields ->
                let result = Map.ofList fields
                if result.Count = fields.Length
                then lift result
                else fail "Duplicate record fields are not allowed"
    let recordDefinition : Parser<LanguageStatement> = typeName .>>. recordFieldsDefinition |>> (RecordDefinition >> TypeDefinition)
    
    let expression, expressionRef = createParserForwardedToRef ()
    
    let assignment : Parser<LanguageStatement> = letKeyword >>. identifier .>> equality .>>. expression |>> Assignment
    let statement : Parser<LanguageStatement> =
        spaces
        >>. choice [
            recordDefinition
            assignment
        ]
        .>> endOfLine
        .>> spaces
        
    let languageConstruct : Parser<LanguageConstruct> = (statement |>> Statement) <|> (expression |>> Expression)
    let program : Parser<LanguageConstruct NonEmptyList> = many1 languageConstruct
    
    let constant : Parser<LanguageExpression> = intLiteral <|> stringLiteral <|> unitLiteral |>> Constant
    let variable : Parser<LanguageExpression> = identifier |>> Variable
    
    let recordFieldAssignment = identifier .>> equality .>>. expression
    let recordConstructor : Parser<LanguageExpression> =
        spaces >>. curlyOpen >>. spaces
        >>. (recordFieldAssignment |> sepBy fieldSeparator)
        .>> spaces .>> curlyClose
        >>= fun fields ->
                let result = Map.ofList fields
                if result.Count = fields.Length
                then lift result
                else fail "Duplicate record field assignments are not allowed"
        |>> RecordConstructor
    
    let propertyAccess : Parser<LanguageExpression> = identifier .>> dot .>>. identifier |>> PropertyAccess
    let list : Parser<LanguageExpression> = bracketOpen >>. many expression .>> bracketClose |>> List
    
    let functionalParameter : Parser<FunctionParameter> =
        unitLiteral |>> fun _ -> UnitLiteral
        <|> (identifier |>> Inferred)
        <|> (parenOpen >>. identifier .>> colon .>>. compileTimeType .>> parenClose |>> Implicit)
        .>> spaces
    let functionalParameters : Parser<FunctionParameters> = many1 functionalParameter |>> Parameters
    
    let lambda : Parser<LanguageExpression> =
        funKeyword >>. spaces1
        >>. functionalParameters .>> spaces
        .>> arrow
        .>>. many languageConstruct
        .>> spaces .>> returnKeyword .>>. expression
        >>= fun ((parameters, constructs), final) -> lift <| Lambda (parameters, constructs, final)
    
    do expressionRef.Value <-
        lineSpaces
        >>. choice [
            constant
            propertyAccess
            variable
            recordConstructor
            list
            lambda
        ]
        .>> lineSpaces