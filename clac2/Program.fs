
open Clac2.Utilities
open Clac2.Modularization
open Clac2.MiddleEnd
open Clac2.Evaluator
open FSharp.Core.Result
open Clac2.Language

[<EntryPoint>]
let main args =
    let baseFuncsCombined = Array.concat [BuildIn.baseFuncs; BuildIn.baseVars]
    let stdCtx = StandardContext.buildStandardContext baseFuncsCombined Types.supportedTypes Syntax.commentIdentifer

    args
    |> getInput
    |> FileLoading.loadAndParseFiles stdCtx
    |> bind (TypeChecking.checkTypes stdCtx)
    |> bind (fun program -> program.mainFile.lines |> evaluateLines stdCtx |> combineResultsToArray) // consider other files too here
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results |> Array.iter (printfn "%A")

        | Error e ->
            printfn "Error:\n%s" e

        x
    )
    |> resultToReturnCode

