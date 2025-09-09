namespace ParserCombinatorsFsharp

open System

type Parser<'a> = Input -> Result<'a>

module Parser =
    exception NonConsumingParsersAreNotAllowed
    
    let lift value = fun input -> Success <| { Value = value; RemainingInput = input }
    let fail error = fun (_ : Input) -> Error error |> Fail
    let map (f : 'a -> 'b) (parser : Parser<'a>) : Parser<'b> = fun input -> input |> parser |> Result.map f
    let (<!>) = map
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
        | false -> Result.fail $"'exact' expected {searchTerm} at position {input.CurrentPosition}"
    
    let satisfy f = fun (input : Input) ->
        let rest = input.Rest
        if rest.Length > 0 && f rest[0] then
            Success <| { Value = rest[0]; RemainingInput = input.Consume(1) }
        else
            Result.fail $"'satisfy' expected a specific character at {input.CurrentPosition}"
    
    let choice (parsers : Parser<'a> list) : Parser<'a> = fun (input : Input) -> 
        parsers
        |> List.tryPick (fun parser -> match parser input with
                                       | Success parsed -> Some <| Success parsed
                                       | Fail _ -> None)
        |> Option.defaultValue (Result.fail $"'choice' expected at least one of the parsers to work at {input.CurrentPosition}")
    
    let (<|>) p1 p2 : Parser<'a> = choice [p1; p2]
    
    let (>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) = p1 >>= fun _ -> p2
    
    let (.>>) (p1 : Parser<'a>) (p2 : Parser<'b>) = p1 >>= fun result -> (fun _ -> result) <!> p2
    
    let (.>>.) (p1 : Parser<'a>) (p2 : Parser<'b>) =
        p1 >>= fun result1 -> (fun result2 -> (result1, result2)) <!> p2
    
    // definitely not the most optimal way to do it
    let many (parser : Parser<'a>) : Parser<'a list> = fun (input : Input) ->
        try
            let noneConsumed = { Value = []; RemainingInput = input }
            noneConsumed
            |> Seq.unfold (fun state -> match parser state.RemainingInput with
                                         | Success success ->
                                             let newState = { Value = success.Value::state.Value; RemainingInput = success.RemainingInput }
                                             if state.RemainingInput.CurrentPosition <> newState.RemainingInput.CurrentPosition
                                             then Some (newState, newState)
                                             else raise NonConsumingParsersAreNotAllowed
                                         | Fail _ -> None)
            |> Seq.tryLast
            |> Option.map (fun parsed -> { parsed with Value = List.rev parsed.Value })
            |> Option.defaultValue noneConsumed
            |> Result<'a list>.Success
        with
        | NonConsumingParsersAreNotAllowed -> Result.fail "'many' does not support non-consuming parsers"
        
    let many1 (parser : Parser<'a>) : Parser<'a list> =
        many parser >>= function
                        | [] -> fail "'many1' expected a parser to work at least once"
                        | x -> lift <| x
    
    let many1Char (parser : Parser<char>) : Parser<string> =
        fun (result : char list) -> String.Join("", Seq.ofList result)
        <!> many1 parser

    let many1Char2 (p1 : Parser<char>) (p2 : Parser<char>) : Parser<string> =
        p1 >>= fun firstChar -> 
               fun (result : char list) -> String.Join("", Seq.ofList <| firstChar::result)
               <!> many1 p2
    
    let spaces = fun input -> ignore <!> (many <| satisfy Char.IsWhiteSpace) <| input
    let spaces1 = fun input -> ignore <!> (many1 <| satisfy Char.IsWhiteSpace) <| input
    
    let between (pOpen : Parser<'a>) (pClose : Parser<'b>) (parser : Parser<'c>) : Parser<'c> =
        pOpen
        >>= fun _ -> parser
        >>= fun result -> (fun _ -> result)
                          <!> pClose
    
    let isDigit = satisfy Char.IsDigit
    let isLetter = satisfy Char.IsLetter
    let isAlphanumeric = fun input -> isDigit <|> isLetter <| input