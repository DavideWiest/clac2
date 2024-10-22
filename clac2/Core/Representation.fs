module rec Clac2.Core.Representation

open Clac2.Core.Domain
open Clac2.Core.Exc.Domain
open Clac2.Core.Lang.Language

module Print =

    let sepatator () = printfn "%s" "-----"
    let printNested nestLevel s = printfn "%s%s" (String.replicate nestLevel "    ") s

    let program (program: Program) =
        sepatator ()
        printfn "Program:"
        printfn "Main file (%s): " (ToString.fileLocOption program.mainFile.maybeLocation)
        orderedFileShortened true 1 program.mainFile.content
        printfn "Secondary files: "
        for file in program.secondaryFiles do
            printNested 1 (sprintf "File (%s): " file.location)
            orderedFileShortened false 2 file.content
        sepatator ()

    let orderedFileShortened printExprs nestLevel (file: OrderedFile) =
        if file.typeDefinitions.Length > 0 then printNested nestLevel Syntax.defineTypeKeywoerd
        file.typeDefinitions |> Array.iter (typeDefShortened (nestLevel + 1))

        if file.assignments.Length > 0 then printNested nestLevel Syntax.assignKeyword
        file.assignments |> Array.iter (printAssignments (nestLevel + 1))

        if printExprs then
            printNested nestLevel "Expressions: "
            file.expressions |> Array.iter (freeManipShortened (nestLevel + 1))

    let typeDefShortened nestLevel (t: TypeDefinition) =
        printNested nestLevel (t.name + ": " + ToString.signature t.signature)

    let printAssignments nestLevel (a: CallableFunction) =
        printNested nestLevel (a.name + ": " + ToString.signature a.signature)
        printNested (nestLevel + 1) (ToString.manip a.manip)

    let freeManipShortened nestLevel (fm: FreeManipulation) =
        printNested (nestLevel + 1) (fm.manip |> ToString.manip)

    let printFullExc callDir (e: FullExc) =
        let relFilePath path = System.IO.Path.GetRelativePath (callDir,path)
        let (fileSubStr, lineSubStr, fileLink) = ToString.locParts e.innerExc.line e.fileLocation relFilePath
        printfn "Error occured%s%s: %s" fileSubStr lineSubStr fileLink
        if e.maybeTrace.IsSome then
            for loc in e.maybeTrace.Value do
                let (subFileSubStr, subLineSubStr, subFileLink) = ToString.locParts (Some loc.lineLocation) loc.fileLocation relFilePath
                printfn "   - %s%s: %s" subFileSubStr subLineSubStr subFileLink
        printfn "%s" e.innerExc.innerExc.message

    let printResult v = 
        printfn "%A" v

module ToString =
    let rec manip (m: Manipulation) =
        m 
        |> Array.map (fun x -> 
            match x with
            | Fn f -> f
            | Manipulation m' -> "(" + manip m' + ")"
        ) 
        |> String.concat " "

    let rec signature s =
        s
        |> Array.map (fun x -> 
            match x with
            | BaseFnType s -> s
            | Function fs -> "(" + signature fs + ")"
        ) 
        |> String.concat " "

    let locParts maybeLine maybeFile relFilePath = 
        let lineSubStr = if maybeLine.IsSome then sprintf " at line %d" (maybeLine.Value+1) else ""
        let fileSubStr = " in " + fileLocOption (maybeFile |> Option.map relFilePath)
        let fileLink = 
            match (maybeFile, maybeLine) with
            | Some fileLoc, Some lineLoc -> sprintf "%s:%d" fileLoc (lineLoc+1)
            | Some fileLoc, None -> fileLoc
            | _ -> ""

        fileSubStr, lineSubStr, fileLink

    let fileLocOption maybeFileLoc = maybeFileLoc |> Option.defaultValue ("interactive")