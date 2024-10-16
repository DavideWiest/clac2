module rec Clac2.Interpreter

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result
open Clac2.DomainUtilities

let evaluateFile (stdCtx: StandardContext) (program: Program) : FullClacResult<DefinedValue> array =
    // stdCtx must not be used after following line - its data is incomplete compared to the program and evalCtx
    let evalCtx = EvalCtx.init stdCtx program

    program.mainFile.content.expressions
    |> Array.map (fun freeManip -> evaluateOne evalCtx freeManip.loc freeManip.manip)

let evaluateOne evalCtx loc manipulation substitutions  =
    printfn "evaluateOne for %A" manipulation
    printfn "- subs: %A" substitutions
    let newEvalCtx = { evalCtx with locTrace = loc :: evalCtx.locTrace }
    let maybeSubstitutedManipulation = substituteMany newEvalCtx substitutions manipulation
    printfn "- subsManip: %A" maybeSubstitutedManipulation

    // error here
    // eval has to be called even if m[0] is defined

    maybeSubstitutedManipulation
    |> bind (fun (substitutedManipulation: SubsitutionResult array) ->
        let definedManipulationTail = convertToDefinedManipulation newEvalCtx substitutedManipulation[1..]
        match substitutedManipulation[0] with
        | Ref f -> definedManipulationTail |> bind (eval newEvalCtx f)
        | Val (DefinedPrimitive p) -> if substitutedManipulation.Length = 1 then p |> DefinedPrimitive |> Ok else EvalCtx.FullExcFromEvalCtx ("Primitive " + p.ToString() + " used as function (at interpreter).") newEvalCtx
        | Val (DefinedFn (name, fn)) -> definedManipulationTail |> bind (fun args -> fn args |> toGenericResult |> EvalCtx.toFullExcFromEvalCtx newEvalCtx)
    )

let convertToDefinedManipulation evalCtx substitutedManip : FullClacResult<DefinedValue array> =
    substitutedManip
    |> Array.fold (fun acc r ->
        acc
        |> bind (fun accArray ->
            match r with
            | Val v -> (Array.append accArray [|v|]) |> Ok
            | Ref r -> Error (GenExc ("Reference " + r.ToString() + " could not be substituted (at interpreter).")) |> toIntermediateResult evalCtx.locTrace[0].lineLocation |> toFullResult evalCtx.locTrace[0].fileLocation // f# type interference does not work any other way correctly
        )
    ) (Ok [||])

let substituteMany evalCtx (substitutions: Map<string, DefinedValue>) m = m |> Array.map (substituteOne evalCtx substitutions) |> combineResultsToArray

type SubsitutionResult = Ref of Reference | Val of DefinedValue

let rec substituteOne evalCtx (substitutions: Map<string, DefinedValue>) x : FullClacResult<SubsitutionResult> =
    match x with
    | Fn f -> 
        printfn "substituteOne %s with %A" f substitutions

        if isPrimitive f then f |> readPrimitive |> DefinedPrimitive |> Val |> Ok else
        // substitutions first to override globally defined functions
        if substitutions.ContainsKey f then substitutions[f] |> Val |> Ok else
        // do not pass down defined arguments as substitutions
        if evalCtx.customAssignmentMap.ContainsKey f then evaluateOne evalCtx evalCtx.customAssignmentMap[f].loc evalCtx.customAssignmentMap[f].fn Map.empty |> map Val else
        if evalCtx.stdFunctionsMap.ContainsKey f then toDefinedFn evalCtx f |> Val |> Ok else
        
        Ref x |> Ok
    | Manipulation m -> evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx) m substitutions |> map Val


type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, BuiltInFn>
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

    let toFullExcFromEvalCtx (evalCtx: EvalCtx) (result: Result<'a,string>): FullClacResult<'a> =
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        result |> toGenericResult |> toIntermediateResult lastExc.lineLocation |> toFullResult lastExc.fileLocation |> addLocTraceToResult trace

    let FullExcFromEvalCtx (evalCtx: EvalCtx) (e: string) = 
        let trace, lastExc = splitToTraceAndLastExc evalCtx
        e |> GenExc |> IntermediateExc (lastExc.lineLocation) |> FullExc lastExc.fileLocation |> addLocTraceToExc evalCtx.locTrace |> Error

    let splitToTraceAndLastExc evalCtx = evalCtx.locTrace[1..], evalCtx.locTrace[0]

    let getCurrentLoc evalCtx = evalCtx.locTrace[0]