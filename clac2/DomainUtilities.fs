module rec Clac2.DomainUtilities 

open System
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
    else Error (GenExc ("Function not found (at evaluation): " + f))

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
let IntermediateExcWithoutLine (e: GenericException) = { genExc = e; line = None}
let IntermediateExcMaybeLine maybeLine (e: GenericException) = { genExc = e; line = maybeLine }
let IntermediateExcFromParts (e: string) (line: int) = e |> GenExc |> IntermediateExc line
let IntermediateExcFPPure (e: string) (line: int) = e |> GenExc |> IntermediateExc line
let IntermediateExcFPPureMaybeLine (e: string) maybeLine = e |> GenExc |> IntermediateExcMaybeLine maybeLine

let toIntermediateResult (line: int) (result: GenericResult<'a>) : IntermediateClacResult<'a> =
    result |> mapError (fun e -> IntermediateExc line e)

let toIntermediateResultWithoutLine (result: GenericResult<'a>) : IntermediateClacResult<'a> = 
    result |> mapError IntermediateExcWithoutLine

let tupledToIntermediateResult (result: int * GenericResult<'a>) : IntermediateClacResult<'a> = 
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

let printFullExc callDir (e: FullGenericException) =
    let relFilePath path = System.IO.Path.GetRelativePath (callDir,path)
    let lineSubStr = if e.genExcWithLine.line.IsSome then sprintf " at line %d" (e.genExcWithLine.line.Value+1) else ""
    let fileSubStr = " in " + fileLocOptionToString (e.fileLocation |> Option.map relFilePath)
    let fileLink = 
        match (e.fileLocation, e.genExcWithLine.line) with
        | Some fileLoc, Some lineLoc -> sprintf "%s:%d" fileLoc (lineLoc+1)
        | Some fileLoc, None -> fileLoc
        | _ -> ""
    printfn "Error occured%s%s: %s" fileSubStr lineSubStr fileLink
    printfn "%s" e.genExcWithLine.genExc.message

let fileLocOptionToString maybeFileLoc = maybeFileLoc |> Option.defaultValue ("interactive")

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

let printProgram (program: Program) =
    printfn "%s" "---"
    printfn "%s" "Program"
    printfn "Main file: %A" program.mainFile
    printfn "Secondary files: "
    for file in program.secondaryFiles do
        printfn "- %A" file
    printfn "---"

let rec manipulationToString (m: Manipulation) =
    m 
    |> Array.map (fun x -> 
        match x with
        | Fn f -> f
        | Manipulation m' -> "(" + manipulationToString m' + ")"
    ) 
    |> String.concat " "

// type and primitive functions

let definedValueToInt (x: DefinedValue) = definedValueToIntInner x 0

let rec definedValueToIntInner (x: DefinedValue) (recursionCount: int) =
    // prevent infinite recursion while allowing users to pass nested functions
    if recursionCount > 10 then Error (sprintf "Recursion limit reached while trying to convert argument to int for: %A" x) else

    match x with
        | DefinedPrimitive (PrimitiveInt i) -> Ok i
        | DefinedFn (name, fn) ->
            match fn [| |] with
            | Ok(v) -> definedValueToIntInner v (recursionCount + 1)
            | Error e -> Error e

let readPrimitive (p: string) =
    // ints
    if Seq.forall Char.IsDigit p then p |> int |> PrimitiveInt else

    failwith ("Unable to parse primitive: " + p)

let isPrimitive (s: string) = 
    // ints
    Seq.forall Char.IsDigit s

let getValidatedPrimitiveType primitiveStr =
    match readPrimitive primitiveStr with
        | PrimitiveInt p -> BaseFnType "int"

// string

let getInput (args: string array) =
    if args.Length = 0 then
        printf "Enter the program:\n"
        Interactive (Console.ReadLine())
    else
        File args[0]

// misc base

let resultToReturnCode x =
    match x with 
    | Ok _ -> 0 
    | Error _ -> 1

let lineToString (line: Line) =
    let exprToString m = m |> Array.map string |> String.concat " "

    match line with
    | Expression m -> exprToString m.manipulation
    | Assignment f -> 
        let args = (f.args |> String.concat " ")
        let signature = (f.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")
        let fnBody = exprToString f.fn

        sprintf "let %s %s : %s = %s" f.name args signature fnBody
    | TypeDefinition t -> 
        let signature = (t.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")

        sprintf "type %s : %s" t.name signature
    | ModuleReference m -> sprintf "open %s" m
    | ModuleDeclaration m -> sprintf "module %s" m
