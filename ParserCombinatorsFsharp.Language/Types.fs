namespace ParserCombinatorsFsharp.Language

open System
open System.Text.RegularExpressions
open ParserCombinatorsFsharp

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
    | Unit

type CompileTimeType =
    | Unit
    | Integer
    | String
    | Custom of Identifier

type FunctionParameter =
    | UnitLiteral
    | Inferred of Identifier
    | Implicit of Identifier * CompileTimeType

type FunctionParameters = | Parameters of FunctionParameter NonEmptyList

// I allow empty records just for the sake of simplicity
type RecordDefinition = Identifier * Map<Identifier, CompileTimeType>

type TypeDefinition = | RecordDefinition of RecordDefinition

// I've decided to distinct expressions from statements in my approach, something that Ballerina doesn't do.

// I believe this distinction is important: e.g. Ballerina defines record as | Record of List<TypeExpr * TypeExpr>
// which tells me it's possible to have a Let (assignment I presume) as a part of record construction on either side
// (AST-wise at least, it's probably excluded later by a syntax check).
// Imho it's quite important to design types as restrictive as possible to ease later development.
// Although I don't have a lot of experience defining languages so I might be wrong

// Statements are something non-returning, like type declarations, variable assignments, etc.
type LanguageStatement =
    | TypeDefinition of TypeDefinition
    | Assignment of Identifier * LanguageExpression

// Expressions are more of a classic AST and supposed to return something.
// If you are looking for something similar to Ballerina.ExprType, this is your guy
and LanguageExpression =
    | Constant of Literal
    | Variable of Identifier
    | RecordConstructor of Map<Identifier, LanguageExpression>
    | PropertyAccess of Identifier * Identifier
    | List of LanguageExpression list
    | Lambda of FunctionParameters * LanguageConstruct list * LanguageExpression

and LanguageConstruct =
    | Statement of LanguageStatement
    | Expression of LanguageExpression