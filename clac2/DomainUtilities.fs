module rec Clac2.DomainUtilities 

open Clac2.Domain
open FSharp.Core.Result

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
    currentLoc: ProgramLocation
}

module EvalCtx =
    let init stdCtx program loc  =        
        let file = program.mainFile
        let allFiles = 
            program.secondaryFiles
            |> Array.map (fun x -> x.content) |> Array.append [| file.content |]

        printfn "allFiles: %A" allFiles

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

let GenExcError (e: string) = e |> GenExc |> Error

let toGenericResult (result: Result<'a, string>) : GenericResult<'a> =
    match result with
    | Ok v -> Ok v
    | Error e -> GenExcError e

// IntermediateGenericException and IntermediateClacResult

let IntermediateExc (line: int) (e: GenericException) = { genExc = e; line = Some line }
let IntermediateExcFromParts (e: string) (line: int) = e |> GenExc |> IntermediateExc line |> Error
let IntermediateExcFPPure (e: string) (line: int) = e |> GenExc |> IntermediateExc line

let toIntermediateResult (line: int) (result: GenericResult<'a>) : IntermediateClacResult<'a> =
    result |> mapError (fun e -> IntermediateExc line e)

let toIntermediateExcWithoutLine (result: GenericResult<'a>) : IntermediateClacResult<'a> = 
    result |> mapError (fun e -> { genExc = e; line = None })

let tupledToIntermediateGenExc (result: int * GenericResult<'a>) : IntermediateClacResult<'a> = 
    let (line, err) = result
    toIntermediateResult line err


// FullGenericException and FullClacResult

let FullExc (location: string option) (genExcWithLine: IntermediateException) = { genExcWithLine = genExcWithLine; fileLocation = location }
let FullExcFromParts (e: string) (line: int) (location: string option) = e |> GenExc |> IntermediateExc line |> FullExc location |> Error

let toFullResult (location: string option) (result: IntermediateClacResult<'a>) : FullClacResult<'a> =
    result |> mapError (fun e -> FullExc location e)

let toFullExcFromEvalCtx (evalCtx: EvalCtx) (result: GenericResult<'a>) : FullClacResult<'a> =
    result |> toIntermediateResult evalCtx.currentLoc.lineLocation |> toFullResult evalCtx.currentLoc.fileLocation

let FullExcFromEvalCtx (e: string) (evalCtx: EvalCtx) = e |> GenExc |> IntermediateExc (evalCtx.currentLoc.lineLocation) |> FullExc evalCtx.currentLoc.fileLocation |> Error

let tupledToFullExc (result: string option * IntermediateClacResult<'a>) : FullClacResult<'a> = 
    let (location, err) = result
    toFullResult location err

let printFullExc (e: FullGenericException) =
    let lineSubStr = if e.genExcWithLine.line.IsSome then sprintf "at line %d" (e.genExcWithLine.line.Value+1) else ""
    let fileSubStr = if e.fileLocation.IsSome then sprintf "in %s" e.fileLocation.Value else "in interactive"
    let fileLink = 
        match (e.fileLocation, e.genExcWithLine.line) with
        | Some fileLoc, Some lineLoc -> sprintf "%s:%d" fileLoc (lineLoc+1)
        | Some fileLoc, None -> fileLoc
        | _ -> ""
    printfn "Error occured %s %s: %s" fileSubStr lineSubStr fileLink
    printfn "%s" e.genExcWithLine.genExc.message

// Utilities

let combineClacResultsToArray (results: GenericResult<'a> seq) : GenericResult<'a array> =
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