
open Clac2.Utilities
open Clac2.DomainUtilities
open Clac2.Modularization
open Clac2.Interpreter
open FSharp.Core.Result
open Clac2.Language

// stdCtx for front/middle end and back end should be 2 different types

[<EntryPoint>]
let main args =
    let baseFuncsCombined = Array.concat [BuildIn.arithmeticFuncs; BuildIn.baseVars]
    let stdCtx = StandardContext.buildStandardContext baseFuncsCombined Types.baseTypes 

    args
    |> getInput
    |> FileLoading.loadAndParseFiles stdCtx
    |> bind (TypeChecking.validateProgramTypes stdCtx)
    |> map (passAndReturn printProgram)
    |> bind (fun program -> program |> evaluateFile stdCtx |> combineResultsToArray)
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results |> Array.iter (printfn "%A")

        | Error e -> printFullExc System.Environment.CurrentDirectory e
        
        x
    )
    |> resultToReturnCode

