module rec Clac2.BuiltIn.Interface

open Clac2.Core.Utils
open Clac2.Core.Domain
open FSharp.Core.Result

let fSharpFunctionToFn (typedArgs: (string * FnType) array) returnType baseFnOption (data: string * OperatorFixation * DefinedFn) =
    let name, fixation, f = data
    {
        name = name
        signature = typedArgs |> Array.map snd |> Array.append [| returnType |]
        args = typedArgs |> Array.map fst
        DefinedFn = f
        fnOptions = { baseFnOption with fixation = fixation }
    }

let rec fnAdapter name nArgs conversionFn reconversionFn f (input: DefinedValue array)  =
        if input.Length < nArgs then 
            (name + sprintf " (%i/%i args)" input.Length nArgs, fun args -> fnAdapter name nArgs conversionFn reconversionFn f (Array.append input args))
            |> DefinedFn 
            |> Ok
        else

        input
        |> conversionFn
        |> bind (f >> reconversionFn >> Ok)

