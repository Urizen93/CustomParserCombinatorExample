namespace ParserCombinatorsFsharp

open System
open Helpers
open NonEmptyList

type Parser<'a> = Input -> Result<'a>

module Parser =
    exception NonConsumingParsersAreNotAllowed
    exception NotExpectedToFail
    
    let lift value input = Success <| { Value = value; RemainingInput = input }
    
    let fail (message : string) (input : Input) = Fail { Message = message; Input = input }
    let map (f : 'a -> 'b) (parser : Parser<'a>) input = input |> parser |> Result.map f
    let (|>>) parser f = map f parser
    let join (parser : Parser<Parser<'a>>) input =
        match input |> parser with
        | Success success -> success.Value <| success.RemainingInput
        | Fail error -> Fail error
    let bind (f : 'a -> Parser<'b>) (parser : Parser<'a>) : Parser<'b> = (parser |> map f) |> join
    let (>>=) parser f : Parser<'b> = bind f parser
    let runOnString (parser : Parser<'a>) = StringInput.New >> parser
    
    let exact (searchTerm : string) (input : Input) =
        let mutable result = true 
        for i in 0..searchTerm.Length - 1 do
            if result
            then
                match input.PeekForward i with
                | ValueSome nextSymbol -> result <- nextSymbol = searchTerm[i]
                | ValueNone -> result <- false
            else ()
        if result
        then Success <| { Value = (); RemainingInput = input.Consume(searchTerm.Length) }
        else fail $"'exact' expected {searchTerm}" input
    
    let satisfy predicate (input : Input) =
        match input.Next with
        | ValueSome nextChar when predicate nextChar ->  Success <| { Value = nextChar; RemainingInput = input.Consume(1) }
        | _ -> fail "'satisfy' expected a specific character" input
    
    let anyChar : Parser<char> = satisfy <| fun _ -> true
    
    let exactChar : char -> Parser<char> = satisfy << (=)
    
    let (~%) = exactChar
    
    let choice (parsers : Parser<'a> list) input =
        parsers
        |> List.tryPick (fun parser -> match parser input with
                                       | Success parsed -> Some <| Success parsed
                                       | Fail _ -> None)
        |> Option.defaultValue (fail "'choice' expected at least one of the parsers to work" input)
    
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
    
    let many (parser : Parser<'a>) input =
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
    
    let manyCharTill (parser : Parser<char>) (tillParser : Parser<'a>) input =
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
                    maybeError <- Some <| error.Prepend "'manyCharsTill' failed: "
                    shoudlContinue <- false
        Fail
        <!> maybeError
        <|>% Success { currentState with Value = List.rev currentState.Value }
        |> Result.map (fun chars -> String.Join("", chars))
    
    let manyCharBetween2 boundary chars = boundary >>. manyCharTill chars boundary
        
    let many1 (parser : Parser<'a>) : Parser<'a NonEmptyList> =
        many parser >>= function
                        | [] -> fail "'many1' expected a parser to work at least once"
                        | x::xs -> lift <| NonEmptyList (x, xs)
    
    let many1Char (parser : Parser<char>) : Parser<string> =
        many1 parser
        |>> fun (NonEmpty result) -> String.Join("", result)

    let many1Char2 (p1 : Parser<char>) (p2 : Parser<char>) : Parser<string> =
        p1 >>= fun firstChar ->
               many p2
               |>> fun result -> String.Join("", firstChar::result)
    
    let spaces : Parser<unit>  = many <| satisfy Char.IsWhiteSpace |>> ignore
    let spaces1 : Parser<unit> = many1 <| satisfy Char.IsWhiteSpace |>> ignore
    let lineSpaces: Parser<unit> = many <| satisfy (fun ch -> Char.IsWhiteSpace ch && ch <> '\n') |>> ignore
    let newLine : Parser<unit> = %'\n' |>> ignore
    let endOfInput : Parser<unit> = fun (input : Input) ->
        if input.IsAtTheEnd
        then Success <| { Value = (); RemainingInput = input }
        else fail "End of input is expected yet symbols still remain" input
    let endOfLine : Parser<unit> = manyCharTill (satisfy Char.IsWhiteSpace) (newLine <|> endOfInput) |>> ignore
    
    let between (pOpen : Parser<'a>) (pClose : Parser<'b>) (parser : Parser<'c>) : Parser<'c> =
        pOpen
        >>= fun _ -> parser
        >>= fun result -> pClose |>> (fun _ -> result)
            
    let optionallyEndsWith (ending : Parser<'b>) (parser : Parser<'a>) input =
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
    let isAlphanumeric : Parser<char> = isDigit <|> isLetter