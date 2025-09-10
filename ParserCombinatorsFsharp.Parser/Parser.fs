namespace ParserCombinatorsFsharp

open System
open Helpers

type Parser<'a> = Input -> Result<'a>

module Parser =
    exception NonConsumingParsersAreNotAllowed
    exception NotExpectedToFail
    
    let lift value = fun input -> Success <| { Value = value; RemainingInput = input }
    
    let failInput (inputBasedError : Input -> string) = fun (input : Input) -> inputBasedError input |> Error |> Fail
    let fail error = failInput (fun _ -> error)
    let map (f : 'a -> 'b) (parser : Parser<'a>) : Parser<'b> = fun input -> input |> parser |> Result.map f
    let (|>>) parser f = map f parser
    let join (parser : Parser<Parser<'a>>) : Parser<'a> =
        fun input ->
        match input |> parser with
        | Success success -> success.Value <| success.RemainingInput
        | Fail error -> Fail error
    let bind (f : 'a -> Parser<'b>) (parser : Parser<'a>) : Parser<'b> = (parser |> map f) |> join
    let (>>=) parser f : Parser<'b> = bind f parser
    let runOnString (parser : Parser<'a>) = Input.New >> parser
    
    let exact (searchTerm : string) = fun (input : Input) ->
        match input.Rest.StartsWith searchTerm with
        | true -> Success <| { Value = (); RemainingInput = input.Consume(searchTerm.Length) }
        | false -> Result.fail $"'exact' expected {searchTerm} at position {input.CurrentPosition}: {input.Rest}"
    
    let satisfy f = fun (input : Input) ->
        let rest = input.Rest
        if rest.Length > 0 && f rest[0] then
            Success <| { Value = rest[0]; RemainingInput = input.Consume(1) }
        else
            Result.fail $"'satisfy' expected a specific character at {input.CurrentPosition}: {input.Rest}"
    
    let anyChar : Parser<char> = satisfy (fun _ -> true)
    
    let exactChar : char -> Parser<char> = satisfy << (=)
    
    let (~%) = exactChar
    
    let choice (parsers : Parser<'a> list) : Parser<'a> = fun (input : Input) -> 
        parsers
        |> List.tryPick (fun parser -> match parser input with
                                       | Success parsed -> Some <| Success parsed
                                       | Fail _ -> None)
        |> Option.defaultValue (Result.fail $"'choice' expected at least one of the parsers to work at {input.CurrentPosition}: {input.Rest}")
    
    let (<|>) p1 p2 : Parser<'a> = choice [p1; p2]
    
    let (>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) = p1 >>= fun _ -> p2
    
    let (.>>) (p1 : Parser<'a>) (p2 : Parser<'b>) =
        p1
        >>= fun result ->
            p2 |>> fun _ -> result
    
    let (.>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) =
        p1
        >>= fun result1 ->
            p2 |>> fun result2 -> (result1, result2)
    
    let many (parser : Parser<'a>) : Parser<'a list> = fun (input : Input) ->
        let mutable currentState = { Value = []; RemainingInput = input }
        let mutable shoudlContinue = true
        while shoudlContinue do
            match parser currentState.RemainingInput with
            | Success success ->
                if currentState.RemainingInput.CurrentPosition <> success.RemainingInput.CurrentPosition
                then currentState <- { Value = success.Value::currentState.Value; RemainingInput = success.RemainingInput }
                else failwith "'many' does not support non-consuming parsers"
            | Fail _ -> shoudlContinue <- false
        Success { currentState with Value = List.rev currentState.Value }
    
    let manyCharTill (parser : Parser<char>) (tillParser : Parser<'a>) = fun (input : Input) ->
        let mutable currentState = { Value = []; RemainingInput = input }
        let mutable maybeError = None
        let mutable shoudlContinue = true
        while shoudlContinue do
            match tillParser currentState.RemainingInput with
            | Success ending ->
                currentState <- { currentState with RemainingInput = ending.RemainingInput }
                shoudlContinue <- false
            | Fail _ ->
                match parser currentState.RemainingInput with
                | Success success ->
                    if currentState.RemainingInput.CurrentPosition <> success.RemainingInput.CurrentPosition
                    then currentState <- { Value = success.Value::currentState.Value; RemainingInput = success.RemainingInput }
                    else failwith "'manyCharsTill' does not support non-consuming parsers"
                | Fail error ->
                    maybeError <- Some error
                    shoudlContinue <- false
        Fail
        <!> maybeError
        <|>% Success { currentState with Value = List.rev currentState.Value }
        |> Result.map (fun chars -> String.Join("", chars))
    
    let manyCharBetween2 boundary chars = boundary >>. manyCharTill chars boundary
        
    let many1 (parser : Parser<'a>) : Parser<'a list> =
        many parser >>= function
                        | [] -> failInput (fun input -> $"'many1' expected a parser to work at least once at {input.CurrentPosition}: {input.Rest}")
                        | x -> lift <| x
    
    let many1Char (parser : Parser<char>) : Parser<string> =
        many1 parser
        |>> fun result -> String.Join("", result)

    let many1Char2 (p1 : Parser<char>) (p2 : Parser<char>) : Parser<string> =
        p1 >>= fun firstChar ->
               many p2
               |>> fun result -> String.Join("", firstChar::result)
    
    let spaces = fun input -> (many <| satisfy Char.IsWhiteSpace) |>> ignore <| input
    let spaces1 = fun input -> (many1 <| satisfy Char.IsWhiteSpace) |>> ignore <| input
    
    let between (pOpen : Parser<'a>) (pClose : Parser<'b>) (parser : Parser<'c>) : Parser<'c> =
        pOpen
        >>= fun _ -> parser
        >>= fun result -> pClose |>> (fun _ -> result)
            
    let optionallyEndsWith (ending : Parser<'b>) (parser : Parser<'a>) : Parser<'a> =
        fun (input : Input) ->
            let matchEnding result =
                match ending result.RemainingInput with
                | Success parsed -> { result with RemainingInput = parsed.RemainingInput }
                | Fail _ -> result
            match parser input with
            | Success parsed -> Success <| matchEnding parsed
            | Fail error -> Fail error
    
    let sepBy (separator : Parser<'b>) (parser : Parser<'a>) : Parser<'a list> =
        parser |> optionallyEndsWith separator |> many
    
    let isDigit = satisfy Char.IsDigit
    let isLetter = satisfy Char.IsLetter
    let isAlphanumeric = fun input -> isDigit <|> isLetter <| input