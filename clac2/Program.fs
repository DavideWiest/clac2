
open Clac2.Utilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open Clac2.Evaluator
open FSharp.Core.Result
open Clac2.Language

[<EntryPoint>]
let main (args) =
    let baseFuncsCombined = Array.concat [BuildIn.baseFuncs; BuildIn.baseVars]
    let stdCtx = StandardContext.buildStandardContext baseFuncsCombined Types.supportedTypes Syntax.commentIdentifer

    getInput args
    |> parse stdCtx
    |> (fun x ->
        match x with 
        | Ok lines ->
            printfn "\nParsed:\n"
            lines
            |> Array.map lineToString
            |> Array.iter (printfn "%s")
            printfn "\n---------\n"
        | Error e -> ()

        x
    )
    |> bind (TypeChecking.validateTypes stdCtx)
    |> bind (fun lines -> lines |> evaluateLines stdCtx |> combineResultsToArray)
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