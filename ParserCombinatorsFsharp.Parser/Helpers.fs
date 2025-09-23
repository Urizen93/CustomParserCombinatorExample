namespace ParserCombinatorsFsharp

type NonEmptyList<'a> = | NonEmptyList of 'a * 'a list

module NonEmptyList =
    let createOrNone = function
        | [] -> None
        | x::xs -> Some <| NonEmptyList (x, xs)
        
    let create list =
        match createOrNone list with
        | Some a -> a
        | None -> failwith "List cannot be empty!"
    
    let (|NonEmpty|) (NonEmptyList (x, xs)) = x::xs

module Helpers =
    open Microsoft.FSharp.Core

    let (<!>) = Option.map

    let (<|>%) option resolution = Option.defaultValue resolution option