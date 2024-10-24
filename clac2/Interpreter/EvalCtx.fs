module rec Clac2.Interpreter.EvalCtx

open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedValue>
    locTrace: ProgramLocation list
}

module EvalCtx =
    let init callableCtx program  = 
        {
            customAssignmentMap = 
                program.secondaryFiles
                |> Array.map (fun x -> x.content) 
                |> Array.append [| program.mainFile.content |]
                |> Array.map (fun x -> x.assignments)
                |> Array.map (Array.map (fun x -> x.name, x))
                |> Array.concat
                |> Map.ofArray
            stdFunctionsMap = 
                callableCtx.functions
                |> Array.map (fun x -> x.name, (x.name, x.DefinedFn) |> DefinedFn)
                |> Map.ofArray
            locTrace = []
        }

    let toFullExcFromEvalCtx evalCtx result =
        let lastExc = evalCtx.locTrace[0]
        result |> Simple.toResult |> Intermediate.toResult lastExc.lineLocation |> Full.toResult lastExc.fileLocation |> Full.addLocTraceToResult evalCtx.locTrace

    let FullExcFromEvalCtx e evalCtx = 
        let lastExc = evalCtx.locTrace[0]
        e |> Simple.Exc |> Intermediate.Exc (lastExc.lineLocation) |> Full.Exc lastExc.fileLocation |> Full.addLocTraceToExc evalCtx.locTrace |> Error

    let getCurrentLoc evalCtx = evalCtx.locTrace[0]