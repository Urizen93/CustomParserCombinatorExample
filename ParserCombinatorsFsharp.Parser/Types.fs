namespace ParserCombinatorsFsharp

open System

type Input private (value : string, position : int) =
    member private this.Value = value
    member this.CurrentPosition = position
    member this.IsAtTheEnd = String.IsNullOrWhiteSpace this.Rest
    
    static member New value = Input (value, 0)
    
    member this.Rest = this.Value.Substring this.CurrentPosition
    member this.Next = if this.CurrentPosition < this.Value.Length
                                 then Some <| this.Value[this.CurrentPosition]
                                 else None
    member this.Consume n = Input (value, position + n)
    override this.ToString() = this.Rest

type Error = | Error of string

type Parsed<'a> = {
    Value : 'a
    RemainingInput : Input
}

[<RequireQualifiedAccess>]
module Parsed =
    let map f success = { Value = f success.Value; RemainingInput = success.RemainingInput }

type Result<'a> =
    | Success of Parsed<'a>
    | Fail of Error

[<RequireQualifiedAccess>]
module Result =
    let map f = function
        | Success success -> success |> Parsed.map f |> Success
        | Fail error -> Fail error
    
    let fail value = Error value |> Fail