module rec Clac2.Core.Representation

open Clac2.Core.Domain
open Clac2.Core.Exc.Domain

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
