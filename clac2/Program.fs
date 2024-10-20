
open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Representation
open Clac2.Core.Input
open Clac2.Core.Language
open Clac2.Core.ProgramUtils
open Clac2.MiddleEnd.Normalization
open Clac2.MiddleEnd.TypeChecker
open Clac2.Interpreter.Interpreter
open Clac2.Modularization

// stdCtx for front/middle end and back end should be 2 different types

[<EntryPoint>]
let main args =
    let baseFuncsCombined = Array.concat [BuiltIn.arithmeticFuncs]
    let stdCtx = StandardContext.buildStandardContext baseFuncsCombined Types.baseTypes 

    args
    |> getInput
    |> loadAndParseFiles stdCtx
    |> map (fun (program, depMap) -> applyNormalizationPipeline stdCtx program, depMap)
    |> bind (validateProgramTypes stdCtx)
    |> map (passAndReturn printProgram)
    |> bind (fun program -> program |> evaluateFile stdCtx.definedCtx |> Result.combineToArray)
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results |> Array.iter (printfn "%A")

        | Error e -> printFullExc System.Environment.CurrentDirectory e
        
        x
    )
    |> resultToReturnCode
