module rec Clac2.BuiltIn.Util

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Lang.Primitive

let genericFnOptions = { fixation = Prefix; noMemo = false }

module DefinedValue =
    // this could also be recursive for primitives inside a function, but fortuneately it's not needed for nwo
    let toPrimitive defVal =
        match defVal with
        | DefinedPrimitive p -> Ok p
        | _ -> Error (sprintf "Expected primitive, got %A" defVal)

    let toIntArray defVals = 
        defVals 
        |> Array.map DefinedValue.toPrimitive 
        |> Result.combineToArray 
        |> bind (Array.map Primitive.toInt >> Result.combineToArray)

    let toFloatArray defVals = 
        defVals 
        |> Array.map DefinedValue.toPrimitive 
        |> Result.combineToArray 
        |> bind (Array.map Primitive.toFloat >> Result.combineToArray)
