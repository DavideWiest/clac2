module rec Clac2.DomainUtilities 

open System
open Clac2.Domain
open FSharp.Core.Result

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

let Intermediate.Exc (line: int) (e: GenericException) = { genExc = e; line = Some line }
let Intermediate.ExcWithoutLine (e: GenericException) = { genExc = e; line = None}
let Intermediate.ExcMaybeLine maybeLine (e: GenericException) = { genExc = e; line = maybeLine }
let Intermediate.ExcFromParts (e: string) (line: int) = e |> GenExc |> Intermediate.Exc line
let Intermediate.ExcFPPure (e: string) (line: int) = e |> GenExc |> Intermediate.Exc line
let Intermediate.ExcFPPureMaybeLine (e: string) maybeLine = e |> GenExc |> Intermediate.ExcMaybeLine maybeLine

let toIntermediateResult (line: int) (result: GenericResult<'a>) : IntermediateClacResult<'a> = result |> mapError (fun e -> Intermediate.Exc line e)
let toIntermediateResultWithoutLine (result: GenericResult<'a>) : IntermediateClacResult<'a> = result |> mapError Intermediate.ExcWithoutLine

let tupledToIntermediateResult (result: int * GenericResult<'a>) : IntermediateClacResult<'a> = 
    let (line, err) = result
    toIntermediateResult line err

// FullGenericException and FullClacResult

let FullExc (location: string option) (genExcWithLine: Intermediate.Exception) = { genExcWithLine = genExcWithLine; fileLocation = location; locTrace = None }
let FullExcFromParts (e: string) (line: int) (location: string option) = e |> GenExc |> Intermediate.Exc line |> FullExc location |> Error

let toFullResult (location: string option) (result: IntermediateClacResult<'a>) : FullClacResult<'a> = result |> mapError (fun e -> FullExc location e)
let tupledToFullExc (result: string option * IntermediateClacResult<'a>) : FullClacResult<'a> = 
    let (location, err) = result
    toFullResult location err

let addLocTraceToExc locTrace (e: FullGenericException) = { e with locTrace = Some locTrace }
let addLocTraceToResult locTrace (r: FullClacResult<'a>) = r |> mapError (addLocTraceToExc locTrace)

let printFullExc callDir (e: FullGenericException) =
    let relFilePath path = System.IO.Path.GetRelativePath (callDir,path)
    let (fileSubStr, lineSubStr, fileLink) = locPartsToString e.genExcWithLine.line e.fileLocation relFilePath
    printfn "Error occured%s%s: %s" fileSubStr lineSubStr fileLink
    if e.locTrace.IsSome then
        for loc in e.locTrace.Value do
            let (subFileSubStr, subLineSubStr, subFileLink) = locPartsToString (Some loc.lineLocation) loc.fileLocation relFilePath
            printfn "   - %s%s: %s" subFileSubStr subLineSubStr subFileLink
    printfn "%s" e.genExcWithLine.genExc.message

let locPartsToString maybeLine maybeFile relFilePath = 
    let lineSubStr = if maybeLine.IsSome then sprintf " at line %d" (maybeLine.Value+1) else ""
    let fileSubStr = " in " + fileLocOptionToString (maybeFile |> Option.map relFilePath)
    let fileLink = 
        match (maybeFile, maybeLine) with
        | Some fileLoc, Some lineLoc -> sprintf "%s:%d" fileLoc (lineLoc+1)
        | Some fileLoc, None -> fileLoc
        | _ -> ""

    fileSubStr, lineSubStr, fileLink

let fileLocOptionToString maybeFileLoc = maybeFileLoc |> Option.defaultValue ("interactive")

// Utilities

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

let Primitive.isPrim (s: string) = 
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
    | Expression m -> exprToString m.manip
    | Assignment f -> 
        let args = (f.args |> String.concat " ")
        let signature = (f.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")
        let fnBody = exprToString f.manip

        sprintf "let %s %s : %s = %s" f.name args signature fnBody
    | TypeDefinition t -> 
        let signature = (t.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")

        sprintf "type %s : %s" t.name signature
    | ModuleReference m -> sprintf "open %s" m
    | ModuleDeclaration m -> sprintf "module %s" m
