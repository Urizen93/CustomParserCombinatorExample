namespace ParserCombinatorsFsharp.Language

[<RequireQualifiedAccess>]
module internal Keyword =
    [<Literal>]
    let Type = "type"
    
    [<Literal>]
    let Let = "let"
    
    [<Literal>]
    let Unit = "unit"
    
    [<Literal>]
    let String = "string"
    
    [<Literal>]
    let Int = "int"

[<RequireQualifiedAccess>]
module internal Keywords =
    let All = Set.ofList [Keyword.Type; Keyword.Let; Keyword.Unit; Keyword.String; Keyword.Int]