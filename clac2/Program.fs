
open System
open FSharp.Core.Result
open Clac2.Core.Utils
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


[<EntryPoint>]
let main args =
    let definedCtx = DefinedCtx.init allFunctions
    let definitionCtx = DefinitionCtx.init Types.baseTypes allFunctions

    args
    |> getInput
    |> loadAndParseFiles definitionCtx
    |> map (fun (program, depMap) -> applyNormalizationPipeline definedCtx program, depMap)
    |> bind (validateProgramTypes definedCtx definitionCtx)
    |> map (passAndReturn printProgram)
    |> bind (evaluateFile definedCtx >> Result.combineToArray)
    |> map (passAndReturn (Array.iter printResult))
    |> mapError (printFullExc Environment.CurrentDirectory)
    |> resultToReturnCode
