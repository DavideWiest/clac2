
open Clac2.Utilities
open Clac2.DomainUtilities
open Clac2.Modularization
open Clac2.Interpreter
open FSharp.Core.Result
open Clac2.Language

[<EntryPoint>]
let main args =
    let baseFuncsCombined = Array.concat [BuildIn.baseFuncs; BuildIn.baseVars]
    let stdCtx = StandardContext.buildStandardContext baseFuncsCombined Types.supportedTypes 

    args
    |> getInput
    |> FileLoading.loadAndParseFiles stdCtx
    |> bind (TypeChecking.checkTypes stdCtx)
    |> bind (fun program -> program |> evaluateFile stdCtx |> combineResultsToArray)
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results |> Array.iter (printfn "%A")

        | Error e -> printFullExc e
        
        x
    )
    |> resultToReturnCode

