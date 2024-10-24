
open System
open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Representation
open Clac2.Core.Input
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.BuiltIn
open Clac2.Core.ProgramUtils
open Clac2.MiddleEnd.Normalization
open Clac2.MiddleEnd.TypeChecker
open Clac2.Interpreter.Interpreter
open Clac2.Modularization

let executeProgram scopeCtx callableCtx input =
    input
    |> loadAndParseFiles scopeCtx
    |> map (fun (program, depMap) -> applyNormalizationPipeline callableCtx program, depMap)
    |> bind (validateProgramTypes callableCtx scopeCtx)
    // |> map (passAndReturn Print.program)
    |> bind (evaluateFile callableCtx >> Result.combineToArray)
    //|> map (passAndReturn (Array.iter Print.printResult))
    |> mapError (Print.printFullExc Environment.CurrentDirectory)

[<EntryPoint>]
let main args =
    let callableCtx = CallableCtx.init allFunctions
    let scopeCtx = ScopeCtx.init Types.baseTypes allFunctions

    match getInput args with
    | Interactive s -> consoleLoop executeProgram scopeCtx callableCtx s
    | File s -> 
        File s
        |> executeProgram scopeCtx callableCtx
        |> resultToReturnCode
