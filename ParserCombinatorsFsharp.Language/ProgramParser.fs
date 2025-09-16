namespace ParserCombinatorsFsharp.Language

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Parser

module ProgramParser =
    open LanguageParser
    
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
                else failInput <| fun input -> $"Duplicate record fields are not allowed at position {input.CurrentPosition}: {input.Rest}"
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
    let program : Parser<LanguageConstruct list> = many1 languageConstruct
    
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
                else failInput <| fun input -> $"Duplicate record field assignments are not allowed at position {input.CurrentPosition}: {input.Rest}"
        |>> RecordConstructor
    
    let propertyAccess : Parser<LanguageExpression> = identifier .>> dot .>>. identifier |>> PropertyAccess
    let list : Parser<LanguageExpression> = bracketOpen >>. many expression .>> bracketClose |>> List
    
    // TODO we also must count indentation for lambdas - or introduce an ending keyword
    let lambda : Parser<LanguageExpression> =
        funKeyword >>. many1 identifier .>> spaces .>> arrow .>>. many1 languageConstruct
        >>= fun (identifiers, constructs) ->
            match List.last constructs with
            | Expression expression ->
                let constructsExceptLast = constructs |> List.removeAt (constructs.Length - 1)
                lift <| Lambda (identifiers, constructsExceptLast, expression)
            | Statement statement ->
                failInput <| fun input -> $"Last construct in a lambda has to be an expression;
                                            found {statement} at position {input.CurrentPosition}: {input.Rest}"
    
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