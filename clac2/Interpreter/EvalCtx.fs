module rec Clac2.Interpreter.EvalCtx

open Clac2.Core.Domain
open Clac2.Core.Exc.Exceptions

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, string * DefinedFn>
    locTrace: ProgramLocation list
}

module EvalCtx =
    let init definedCtx program  = 
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
                definedCtx.functions
                |> Array.map (fun x -> x.name, (x.name, x.DefinedFn))
                |> Map.ofArray
            locTrace = []
        }

    let toFullExcFromEvalCtx evalCtx result =
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        result |> toGenericResult |> toIntermediateResult lastExc.lineLocation |> toFullResult lastExc.fileLocation |> addLocTraceToResult trace

    let FullExcFromEvalCtx e evalCtx = 
        let _, lastExc = splitToTraceAndLastExc evalCtx
        e |> SimpleExc |> IntermediateExc (lastExc.lineLocation) |> FullExc lastExc.fileLocation |> addLocTraceToExc evalCtx.locTrace |> Error

    let splitToTraceAndLastExc evalCtx = evalCtx.locTrace[1..], evalCtx.locTrace[0]

    let getCurrentLoc evalCtx = evalCtx.locTrace[0]