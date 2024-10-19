module rec Clac2.Interpreter.Interpreter

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Language

let evaluateFile definedCtx program =
    program.mainFile.content.expressions |> Array.map (fun freeManip -> evaluateOne (EvalCtx.init definedCtx program) freeManip.loc freeManip.manip Map.empty)

let evaluateOne oldEvalCtx loc manipulation substitutions  =
    let evalCtx = { oldEvalCtx with locTrace = loc :: oldEvalCtx.locTrace }

    if manipulation.Length = 1 then substituteOne evalCtx substitutions [||]  manipulation[0] else

    manipulation[1..] 
    |> Array.map (substituteOne evalCtx substitutions [||]) 
    |> combineResultsToArray
    |> bind (fun tail ->
        match manipulation[0] with
        | Fn f when substitutions.ContainsKey f -> PrimitiveOrApply evalCtx tail substitutions[f]
        | _ -> substituteOne evalCtx Map.empty tail manipulation[0]
    )

let rec substituteOne evalCtx substitutions args x =
    let applicationDefined fn = DefinedFn (fn.name, fn.DefinedFn) |> (PrimitiveOrApply evalCtx args)
    let applicationCustom (fn: CallableFunction) = evaluateOne evalCtx fn.loc fn.manip (args |> Array.zip fn.args[..args.Length-1] |> Map.ofArray) // in case of curried functions, right?

    match x with
    | Fn f -> toDefinedOrApply evalCtx substitutions f applicationDefined applicationCustom
    | Manipulation m -> evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx) m substitutions |> bind (PrimitiveOrApply evalCtx args)

let toDefinedOrApply evalCtx substitutions f applicationDefined applicationCustom =
    if Primitive.isPrim f then f |> Primitive.readPrimitive |> DefinedPrimitive |> Ok
    elif substitutions.ContainsKey f then substitutions[f] |> Ok
    elif evalCtx.stdFunctionsMap.ContainsKey f then applicationDefined evalCtx.stdFunctionsMap[f]
    elif evalCtx.customAssignmentMap.ContainsKey f then applicationCustom evalCtx.customAssignmentMap[f] else
    EvalCtx.FullExcFromEvalCtx ("Function not found (at evaluation): " + f) evalCtx

let PrimitiveOrApply evalCtx args x =
    match x with
    | DefinedPrimitive p -> DefinedPrimitive p |> Ok
    | DefinedFn (_, fn) -> fn args |> EvalCtx.toFullExcFromEvalCtx evalCtx

// ----

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
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
                |> Array.map (fun x -> x.name, x)
                |> Map.ofArray
            locTrace = []
        }

    let toFullExcFromEvalCtx evalCtx result =
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        result |> toGenericResult |> toIntermediateResult lastExc.lineLocation |> toFullResult lastExc.fileLocation |> addLocTraceToResult trace

    let FullExcFromEvalCtx e evalCtx = 
        let _, lastExc = splitToTraceAndLastExc evalCtx
        e |> GenExc |> IntermediateExc (lastExc.lineLocation) |> FullExc lastExc.fileLocation |> addLocTraceToExc evalCtx.locTrace |> Error

    let splitToTraceAndLastExc evalCtx = evalCtx.locTrace[1..], evalCtx.locTrace[0]

    let getCurrentLoc evalCtx = evalCtx.locTrace[0]