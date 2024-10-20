module rec Clac2.BuiltIn.Util

open Clac2.Core.Utils
open Clac2.Core.Domain
open FSharp.Core.Result

module Conversion = 
    let fSharpFunctionToFn (typedArgs: (string * FnType) array) returnType baseFnOption (extraData: string * OperatorFixation * DefinedFn) =
        let name, fixation, f = extraData
        {
            name = name
            signature = typedArgs |> Array.map snd |> Array.append [| returnType |]
            args = typedArgs |> Array.map fst
            DefinedFn = f
            fnOptions = { baseFnOption with fixation = fixation }
        }

    let rec fnTypeToTAdapter po name (f: int array -> int) (input: DefinedValue array) nArgs  =
        if input.Length > nArgs then Error ("Internal Error: Wrong number of arguments for fnTypeToIntAdapter for builtin function " + name) else

        if input.Length < nArgs then 
            Ok (DefinedFn (name + sprintf " (%i/%i args)" input.Length nArgs, fun args -> fnTypeToTAdapter po name f (Array.append input args) nArgs))
        else

        input 
        |> Array.map (Conversion.definedValueToT po) 
        |> Result.combineToArray
        |> bind (f >> PInt >> DefinedPrimitive >> Ok)

    let definedValueToT po v = definedValueToIntInner po v 0

    let rec definedValueToIntInner po v (recursionCount: int) =
        // prevent infinite recursion while allowing users to pass nested functions
        if recursionCount > 10 then Error (sprintf "Recursion limit reached while trying to convert argument to int for: %A" v) else

        match v with
            | DefinedPrimitive dp -> getPrimOption po dp
            | DefinedFn (name, fn) ->
                match fn [| |] with
                | Ok(v) -> definedValueToIntInner po v (recursionCount + 1)
                | Error e -> Error e

    let getPrimOption po dp  =
        match po, dp with
        | PoInt, PInt i -> Ok i
        | PoFloat, PFloat f -> Ok f
        | _ -> Error (sprintf "Expected %A, got %A" po dp)