module rec Clac2.Interpreter

open Clac2.Domain
open Clac2.Utilities
open Clac2.Language
open FSharp.Core.Result
open Clac2.DomainUtilities

type EvalStartFn = 
    | StartReference of Reference
    | DefinedStartFn of string * DefinedFn

let evaluateFile (stdCtx: StandardContext) (program: Program) : FullClacResult<DefinedValue> array =
    let dummyLoc = { fileLocation = None; lineLocation = 0 }
    // stdCtx must not be used after following line - its data is incomplete compared to the program and evalCtx
    let evalCtx = EvalCtx.init stdCtx program dummyLoc

    printfn "%A" program.mainFile.content.expressions

    program.mainFile.content.expressions
    |> Array.map (fun freeManip -> evaluateOne evalCtx freeManip.loc freeManip.manipulation Map.empty)

let evaluateOne evalCtx loc manipulation substitutions  =
    printfn "evaluateOne for %A" manipulation
    printfn "- subs: %A" substitutions
    let newEvalCtx = { evalCtx with currentLoc = loc }

    if manipulation.Length = 1 then substituteOne newEvalCtx substitutions manipulation[0] else

    let substitutedManipulationTail = substituteMany newEvalCtx substitutions manipulation[1..]
    printfn "- subsManip: %A" substitutedManipulationTail

    substitutedManipulationTail
    |> bind (eval newEvalCtx (StartReference manipulation[0]))

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
        
        FullExcFromEvalCtx ("Function not found (at evaluation during substitution): " + f) evalCtx
    // calls eval over evaluateOne with substitutions
    | Manipulation m -> evaluateOne evalCtx evalCtx.currentLoc m substitutions

let rec eval evalCtx (startFn: EvalStartFn) (args: DefinedValue array) : FullClacResult<DefinedValue> =
    match startFn with
    | StartReference(Fn f) ->
        printfn "--- eval %s with %A ---" f args

        if isPrimitive f then 
            if args.Length = 0 then f |> readPrimitive |> DefinedPrimitive |> Ok else FullExcFromEvalCtx ("Primitive " + f + " used as function (at interpreter).") evalCtx
        else
            if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].DefinedFn args |> toGenericResult |> toFullExcFromEvalCtx evalCtx else
            if evalCtx.customAssignmentMap.ContainsKey f |> not then FullExcFromEvalCtx ("Function " + f + " not found (at interpreter)") evalCtx else

            let fn = evalCtx.customAssignmentMap[f]
            let substitutions = args |> Array.zip fn.args |> Map.ofArray

            evaluateOne evalCtx fn.loc fn.fn substitutions
    | StartReference(Manipulation m) ->
        printfn "--- eval manipulation %A with %A ---" m args

        evaluateOne evalCtx evalCtx.currentLoc m Map.empty
        |> bind (fun newStartFnValue -> 
            match newStartFnValue with 
            | DefinedFn (name, fn) -> 
                let newStartFn = DefinedStartFn (name, fn)
                eval evalCtx newStartFn args
            | DefinedPrimitive p -> FullExcFromEvalCtx ("Primitive " + p.ToString() + " used as function.") evalCtx
        ) 
    | DefinedStartFn (name, fn) -> fn args |> toGenericResult |> toFullExcFromEvalCtx evalCtx
 
let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)
