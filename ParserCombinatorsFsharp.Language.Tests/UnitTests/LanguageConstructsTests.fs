namespace ParserCombinatorsFsharp.Language.Tests

open ParserCombinatorsFsharp
open ParserCombinatorsFsharp.Language
open ParserCombinatorsFsharp.Language.ProgramParser
open Xunit
open ParserCombinatorsFsharp.Parser

type ``program parser tests`` (output : ITestOutputHelper) =
    
    [<Theory; ClassData(typeof<Programs>)>]
    member _.``Should parse a sample program`` (input, expected : LanguageConstruct list)=
        match input |> runOnString program with
        | Success parsed -> Assert.Equal<LanguageConstruct> (expected, parsed.Value)
        | Fail (Error fail) -> output.WriteLine fail

and Programs () as this =
    inherit TheoryData<string, LanguageConstruct list>()
    do
        this.Add("
            type Address = { Street: string
                             ZipCode :int}
            type Customer = { Name : string; Address: Address }

            let taras = { Name = \"Taras\";
                             Address = { Street = \"Marka Radovica\"
                                         ZipCode = 81000} }

            [taras]

            let customerZipCodeLens = fun customer ->
                let customerAddress = customer.Address
                customerAddress.ZipCode",
        [Statement
          (TypeDefinition
             (RecordDefinition
                (Identifier.create "Address",
                 Map.ofList [(Identifier.create "Street", String); (Identifier.create "ZipCode", Integer)])))
         Statement
          (TypeDefinition
             (RecordDefinition
                (Identifier.create "Customer",
                 Map.ofList
                   [(Identifier.create  "Address", Custom (Identifier.create  "Address"));
                    (Identifier.create  "Name", String)])))
         Statement
          (Assignment
             (Identifier.create "taras",
              RecordConstructor
                (Map.ofList
                   [(Identifier.create "Address",
                     RecordConstructor
                       (Map.ofList
                          [(Identifier.create "Street", Constant (Literal.String "Marka Radovica"));
                           (Identifier.create "ZipCode", Constant (Literal.Integer 81000))]));
                    (Identifier.create "Name", Constant (Literal.String "Taras"))])))
         Expression
          (List
            [(Variable (Identifier.create "taras"))])
         Statement
          (Assignment
             (Identifier.create "customerZipCodeLens",
              Lambda
                ([Identifier.create "customer"],
                 [Statement
                    (Assignment
                       (Identifier.create "customerAddress",
                        PropertyAccess (Identifier.create "customer", Identifier.create "Address")))],
                 PropertyAccess (Identifier.create "customerAddress", Identifier.create "ZipCode"))))])