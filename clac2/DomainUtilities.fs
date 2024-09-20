module rec Clac2.DomainUtilities 

open Clac2.Domain
open FSharp.Core.Result

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
    currentLoc: ProgramLocation
}

module EvalCtx =
    let init stdCtx file loc  =        
        {
            customAssignmentMap = 
                file.assignments
                |> Array.map (fun x -> x.name, x) 
                |> Map.ofArray
            stdFunctionsMap = 
                stdCtx.definedCtx.functions 
                |> Array.map (fun x -> x.name, x) 
                |> Map.ofArray
            currentLoc = loc
        }

let getSignature evalCtx f =
    if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].signature |> Ok else 
    if evalCtx.customAssignmentMap.ContainsKey f then evalCtx.customAssignmentMap[f].signature |> Ok
    else Error (GenExc ("Function not found: " + f))

let buildLoc fileLoc lineLoc = { fileLocation = fileLoc; lineLocation = lineLoc }

// Exceptions

// GenericException and ClacResult

let GenExc (e: string) = { message = e }

let ClacError (e: string) = e |> GenExc |> Error

let toClacResult (result: Result<'a, string>) : ClacResult<'a> =
    match result with
    | Ok v -> Ok v
    | Error e -> ClacError e

// IntermediateGenericException and IntermediateClacResult

let IntermediateExc (line: int) (e: GenericException) = { genExc = e; line = Some line }
let IntermediateExcFromParts (e: string) (line: int) = e |> GenExc |> IntermediateExc line |> Error

let toIntermediateExc (line: int) (result: ClacResult<'a>) : IntermediateClacResult<'a> =
    result |> mapError (fun e -> IntermediateExc line e)

let toIntermediateExcWithoutLine (result: ClacResult<'a>) : IntermediateClacResult<'a> = 
    result |> mapError (fun e -> { genExc = e; line = None })

let tupledToIntermediateGenExc (result: int * ClacResult<'a>) : IntermediateClacResult<'a> = 
    let (line, err) = result
    toIntermediateExc line err


// FullGenericException and FullClacResult

let FullExcFromParts (e: string) (line: int) (location: string option) = e |> GenExc |> IntermediateExc line |> FullExc location |> Error

let FullExc (location: string option) (genExcWithLine: IntermediateException) = { genExcWithLine = genExcWithLine; fileLocation = location }
let toFullExc (location: string option) (result: IntermediateClacResult<'a>) : FullClacResult<'a> =
    result |> mapError (fun e -> FullExc location e)

let toFullExcFromEvalCtx (evalCtx: EvalCtx) (result: ClacResult<'a>) : FullClacResult<'a> =
    result |> toIntermediateExc evalCtx.currentLoc.lineLocation |> toFullExc evalCtx.currentLoc.fileLocation

let FullExcFromEvalCtx (e: string) (evalCtx: EvalCtx) = e |> GenExc |> IntermediateExc (evalCtx.currentLoc.lineLocation) |> FullExc evalCtx.currentLoc.fileLocation |> Error

let tupledToFullExc (result: string option * IntermediateClacResult<'a>) : FullClacResult<'a> = 
    let (location, err) = result
    toFullExc location err

let printFullExc (e: FullGenericException) =
    let lineSubStr = if e.genExcWithLine.line.IsSome then sprintf " at line %d" e.genExcWithLine.line.Value else ""
    let fileSubStr = if e.fileLocation.IsSome then sprintf " in file %s" e.fileLocation.Value else ""
    printfn "Error occured in %s %s:" fileSubStr lineSubStr
    printfn "%s" e.genExcWithLine.genExc.message

// Utilities

let combineClacResultsToArray (results: ClacResult<'a> seq) : ClacResult<'a array> =
    results
    |> Array.ofSeq
    |> Array.fold (fun acc r ->
        match acc with
        | Ok accList ->
            match r with
            | Ok v -> Ok (Array.append accList [| v |])
            | Error e -> Error e
        | Error e -> Error e
    ) (Ok [| |])