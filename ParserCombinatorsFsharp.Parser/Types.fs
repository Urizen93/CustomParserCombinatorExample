namespace ParserCombinatorsFsharp

open System

type Input =
    abstract member CurrentPosition : int
    abstract member IsAtTheEnd : bool
    abstract member Next : char ValueOption
    abstract member ReadNext: int -> string
    abstract member Consume : int -> Input
    abstract member Sample : string

type StringInput private (value : string, position : int) =
    [<Literal>]
    let SampleSize = 30
    member private this.Value = value
    static member New value = StringInput (value, 0)
    override this.ToString() = this.Value
    
    interface Input with
        member this.CurrentPosition = position
        member this.IsAtTheEnd = this.Value.Length = (this :> Input).CurrentPosition
        member this.Next =
            let currentPosition = (this :> Input).CurrentPosition
            if currentPosition < this.Value.Length
            then ValueSome <| this.Value[currentPosition]
            else ValueNone
        member this.ReadNext n =
            let currentPosition = (this :> Input).CurrentPosition
            let remainingSymbols = this.Value.Length - currentPosition
            this.Value.Substring (currentPosition, Math.Min(remainingSymbols, n))
        member this.Consume n = StringInput (value, position + n)
        member this.Sample = (this :> Input).ReadNext SampleSize
    

type Error = { Message : string; Input : Input } with
    member this.Prepend (prefix : string) = { this with Message = prefix + this.Message }
    override this.ToString () = $"{this.Message} at position {this.Input.CurrentPosition}: {this.Input.Sample}..."

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