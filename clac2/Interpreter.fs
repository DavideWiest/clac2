module rec Clac2.Interpreter

open Clac2.Domain
open Clac2.Utilities
open Clac2.Language
open FSharp.Core.Result
open Clac2.DomainUtilities

let evaluateFile (stdCtx: StandardContext) (program: Program) : FullClacResult<DefinedValue> array =
    // stdCtx must not be used after following line - its data is incomplete compared to the program and evalCtx
    let evalCtx = EvalCtx.init stdCtx program

    printfn "%A" program.mainFile.content.expressions

    program.mainFile.content.expressions
    |> Array.map (fun freeManip -> evaluateOne evalCtx freeManip.loc freeManip.manipulation Map.empty)

let evaluateOne evalCtx loc manipulation substitutions  =
    printfn "evaluateOne for %A" manipulation
    printfn "- subs: %A" substitutions
    let newEvalCtx = { evalCtx with locTrace = loc :: evalCtx.locTrace }

    if manipulation.Length = 1 then substituteOne newEvalCtx substitutions manipulation[0] else

    let substitutedManipulationTail = substituteMany newEvalCtx substitutions manipulation[1..]
    printfn "- subsManip: %A" substitutedManipulationTail

    substitutedManipulationTail
    |> bind (eval newEvalCtx manipulation[0])

let substituteMany evalCtx (substitutions: Map<string, DefinedValue>) m : FullClacResult<DefinedValue array> = 
    m |> Array.map (substituteOne evalCtx substitutions) |> combineResultsToArray

let rec substituteOne evalCtx (substitutions: Map<string, DefinedValue>) x : FullClacResult<DefinedValue> =
    match x with
    | Fn f -> 
        printfn "substituteOne %s with %A" f substitutions

        if isPrimitive f then f |> readPrimitive |> DefinedPrimitive |> Ok else
        // substitutions first to override globally defined functions
        if substitutions.ContainsKey f then substitutions[f] |> Ok else
        // do not pass down defined arguments as substitutions
        if evalCtx.customAssignmentMap.ContainsKey f then evaluateOne evalCtx evalCtx.customAssignmentMap[f].loc evalCtx.customAssignmentMap[f].fn Map.empty else
        if evalCtx.stdFunctionsMap.ContainsKey f then toDefinedFn evalCtx f |> Ok else
        
        EvalCtx.FullExcFromEvalCtx ("Function not found (at evaluation during substitution): " + f) evalCtx
    // calls eval over evaluateOne with substitutions
    | Manipulation m -> evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx) m substitutions

// TODO: implement argument propagation

let rec eval evalCtx (startFn: Reference) (args: DefinedValue array) : FullClacResult<DefinedValue> =
    match startFn with
    | Fn f ->
        printfn "--- eval %s with %A ---" f args

        if isPrimitive f then 
            if args.Length = 0 then f |> readPrimitive |> DefinedPrimitive |> Ok else EvalCtx.FullExcFromEvalCtx ("Primitive " + f + " used as function (at interpreter).") evalCtx
        else
            if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].DefinedFn args |> toGenericResult |> EvalCtx.toFullExcFromEvalCtx evalCtx else
            if evalCtx.customAssignmentMap.ContainsKey f |> not then EvalCtx.FullExcFromEvalCtx ("Function " + f + " not found (at interpreter)") evalCtx else

            let fn = evalCtx.customAssignmentMap[f]
            if args.Length > fn.signature - 1 then 
                evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx)
                |> bind (fun x ->
                    // todo 
                ) 
            else  
            let substitutions = args |> Array.zip fn.args |> Map.ofArray

            evaluateOne evalCtx fn.loc fn.fn substitutions
    | Manipulation m -> 
        evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx) m Map.empty
        |> bind (fun x -> 
            match x with
            | DefinedFn (name, fn) -> fn args |> toGenericResult |> EvalCtx.toFullExcFromEvalCtx evalCtx
            | DefinedPrimitive _ -> if args.Length = 0 then Ok x else EvalCtx.FullExcFromEvalCtx ("Manipulation " + manipulationToString m + "takes zero additional arguments, but got some (at interpreter).") evalCtx
        )
 
let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
    locTrace: ProgramLocation list
}

module EvalCtx =
    let init stdCtx program  =        
        let file = program.mainFile
        let allFiles = 
            program.secondaryFiles
            |> Array.map (fun x -> x.content) |> Array.append [| file.content |]

        {
            customAssignmentMap = 
                allFiles
                |> Array.map (fun x -> x.assignments)
                |> Array.map (Array.map (fun x -> x.name, x))
                |> Array.concat
                |> Map.ofArray
            stdFunctionsMap = 
                stdCtx.definedCtx.functions 
                |> Array.map (fun x -> x.name, x) 
                |> Map.ofArray
            locTrace = [] // empty - do not pass dummyLoc here
        }

    let getSignature evalCtx f =
        if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].signature |> Ok else 
        if evalCtx.customAssignmentMap.ContainsKey f then evalCtx.customAssignmentMap[f].signature |> Ok
        else Error (GenExc ("Function not found (at evaluation): " + f))

    let toFullExcFromEvalCtx (evalCtx: EvalCtx) result: FullClacResult<'a> =
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        result |> toIntermediateResult lastExc.lineLocation |> toFullResult lastExc.fileLocation |> addLocTraceToResult trace

    let FullExcFromEvalCtx (e: string) (evalCtx: EvalCtx) = 
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        e |> GenExc |> IntermediateExc (lastExc.lineLocation) |> FullExc lastExc.fileLocation |> addLocTraceToExc evalCtx.locTrace |> Error

    let splitToTraceAndLastExc evalCtx = evalCtx.locTrace[1..], evalCtx.locTrace[0]

    let getCurrentLoc evalCtx = evalCtx.locTrace[0]