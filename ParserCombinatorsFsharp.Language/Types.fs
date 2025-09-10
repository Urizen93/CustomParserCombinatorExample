namespace ParserCombinatorsFsharp.Language

open System
open System.Text.RegularExpressions

type Identifier = private | Identifier of string with
    static member create (str : string) =
        Identifier.createOrNone str |> Option.defaultWith (raise << Exception)
    static member createOrNone (str : string) =
        let trimmed = str.Trim()
        if Regex.IsMatch (trimmed, @"^[A-Za-z]\w*")
           && not <| Keywords.All.Contains str
        then Some <| Identifier trimmed
        else None

type Literal =
    | Integer of int
    | String of string

type CompileTimeType =
    | Unit
    | Integer
    | String
    | Custom of Identifier

type RecordDefinition = Identifier * Map<Identifier, CompileTimeType>

type TypeDefinition = | RecordDefinition of RecordDefinition

type LanguageStatement =
    | TypeDefinition of TypeDefinition
    | Assignment of Identifier * LanguageExpression
    
and LanguageExpression =
    | Constant of Literal
    | Variable of Identifier
    | RecordConstructor of Map<Identifier, LanguageExpression>

type Language =
    | Statement of LanguageStatement
    | Expression of LanguageExpression