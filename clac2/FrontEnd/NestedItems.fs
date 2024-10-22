module rec Clac2.FrontEnd.NestedItems

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Lang.Primitive
open Clac2.FrontEnd.Util

type NestedItemArray<'a> = NestedItem<'a> array

type NestedItem<'a> =
    | NestedItem of 'a
    | NestedArray of NestedItem<'a> array

let rec applyToInnerItems f nestedItems = 
    match nestedItems with 
    | NestedItem i -> f i |> NestedItem
    | NestedArray a -> a |> Array.map (applyToInnerItems f) |> NestedArray

let rec combineNestedItemResults nestedItems = 
    match nestedItems with
    // change this - it cant be right
    | NestedItem i ->
        match i with
        | Ok x -> x |> NestedItem |> Ok
        | Error e -> Error e
    | NestedArray a -> a |> Array.map combineNestedItemResults |> Result.combineToArray |> map NestedArray

let rec toFnType definitionContext nestedItems = 
    nestedItems
    |> Array.map (fun x -> 
        match x with 
        | NestedItem i -> i |> stringToType definitionContext
        | NestedArray a -> a |> toFnType definitionContext |> map Function
    )
    |> Result.combineToArray

let rec toManipulation (definitionCtx: DefinitionContext) nestedItems = 
    nestedItems
    |> Array.map (fun x -> 
        match x with 
        | NestedItem reference ->
            if Array.contains reference definitionCtx.functions || Primitive.isPrim reference then
                reference |> Fn |> Ok |> Simple.toResult
            else
                Simple.toExcResult ("Unknown function: " + reference)
        | NestedArray a -> a |> toManipulation definitionCtx |> map Manipulation
    )
    |> Result.combineToArray

let concatLowestLevelItems nestedItems = concatLowestLevelItemsInner nestedItems

let rec concatLowestLevelItemsInner nestedItems =
    nestedItems
    |> Array.collect (fun nestedItems ->
        match nestedItems with
        | NestedItem iArr -> iArr |> Array.map NestedItem
        | NestedArray innerArray ->
            let processedInnerArray = innerArray |> concatLowestLevelItemsInner |> NestedArray
            [| processedInnerArray |]
    )
