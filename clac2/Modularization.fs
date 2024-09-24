module rec Clac2.Modularization

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open FSharp.Core.Result
open Clac2.Language

module FileLoading =

    let loadAndParseFiles (stdCtx: StandardContext) (unparsedMainFile: MainFileUnparsed) : FullClacResult<Program> =
        match unparsedMainFile with
        | Interactive s -> loadAllFilesInner stdCtx Files.standardFileLocations None s
        | File f -> f |> tryReadFileIntermedExc |> toFullResult (Some f) |> loadAllFilesInner stdCtx Files.standardFileLocations (Some f)

    let loadAllFilesInner stdCtx (baseDeps: string array) (mainFileLoc: string option) (mailFileLines: string) : FullClacResult<Program> =
        // fix string vs string option disparity
        // initialize memos with main file ?
        // fileLoc parameter should be string option, mmaybe
        // use  baseDeps
        loadDefCtxFromDependencies Map.empty Map.empty mainFileLoc
        |> bind (fun (mainFileDefCtx, depMemo, fileContentsMemo) ->
            let otherFiles = 
                fileContentsMemo
                |> Map.toArray
                |> Array.map (fun (fileLoc, preParsedLines) ->
                    let defCtx = depMemo[fileLoc]

                    parseFullResult (Some fileLoc) { stdCtx with defCtx = defCtx } preParsedLines
                    |> map (fun orderedFile ->
                        { location = fileLoc; content = orderedFile }
                    )
                )
                |> combineResultsToArray

            let mainFileContent = parseFullResult mainFileLoc { stdCtx with defCtx = mainFileDefCtx } preParsedLines

            match (mainFileContent, otherFiles) with
            | Ok mainFileContent, Ok otherFiles -> 
                Ok {
                    mainFile = {
                        maybeLocation = mainFileLoc
                        content = mainFileContent
                    }
                    secondaryFiles = otherFiles
                }
            | _ -> joinErrorTuple (mainFileContent, otherFiles) |> Error
        )

    type depMemo = Map<string, DefinitionContext>
    type fileContentsMemo = Map<string, (int * UnparsedLine) array>

    let rec loadDefCtxFromDependencies (fileContentsMemo: fileContentsMemo) (depMemo: depMemo) (fileLoc: string) : FullClacResult<DefinitionContext * depMemo * fileContentsMemo> =
        if depMemo.ContainsKey fileLoc then (depMemo[fileLoc], depMemo, fileContentsMemo) |> Ok else

        // if it isnt in depMemo, it isnt in fileContentsMemo
        preparseFile fileLoc 
        |> bind (fun content -> 
            let fileContentsMemoWithThisFile = Map.add fileLoc content fileContentsMemo
            let localDefinedSymbols = extractCustomDefinitions content
            let dependencies = determineDependencies (getDirOfReference (Some fileLoc)) content

            Array.fold (fun acc dep -> 
                acc
                |> bind (fun (accDefCtx, depMemo, fileContentsMemo) ->
                    loadDefCtxFromDependencies fileContentsMemo depMemo dep
                    |> bind (fun (depDefCtx, depMemoNew, fileContentsMemoNew) ->
                        let depMemoNew2 = Map.add dep depDefCtx depMemoNew

                        Ok ({
                            types = Array.concat [accDefCtx.types; depDefCtx.types]; 
                            functions = Array.concat [accDefCtx.functions; depDefCtx.functions]
                        }, depMemoNew2, fileContentsMemoNew)
                    )
                )
            ) (Ok (localDefinedSymbols, depMemo, fileContentsMemoWithThisFile)) dependencies
        )
    
    let determineDependencies (currentDir: string) preParsedLines =
        preParsedLines
        |> Array.choose (fun (i, line) -> match line with | UnparsedModuleReference s -> Some s | _ -> None)
        |> Array.map (Files.toQualifiedFileLoc currentDir)

    let preparseFile (fileLoc: string) =
        fileLoc
        |> tryReadFileIntermedExc
        |> bind preparse
        |> toFullResult (Some fileLoc)
        
    let tryReadFileIntermedExc file : IntermediateClacResult<string> =
        tryReadFile file |> toIntermediateExcWithoutLine

    let tryReadFile file : GenericResult<string> =
        try
            Ok (System.IO.File.ReadAllText file)
        with
        | :? System.IO.FileNotFoundException as e -> GenExcError (sprintf "File not found: %s" file)
        | e -> GenExcError (sprintf "Error reading file \"%s\": %s" file (e.ToString()))

module TypeChecking = 

    let checkTypes stdCtx (program: Program) : FullClacResult<Program> =
        let validateFileArray (fileArr: File array) =
            fileArr
            |> Array.map (fun file -> file.location, file.content)
            |> Array.map (fun (loc, lines) -> Some loc, lines |> TypeChecking.validateTypes stdCtx)
            |> Array.map tupledToFullExc
            |> combineResultsToArray
        
        program.mainFile.content
        |> (TypeChecking.validateTypes stdCtx)
        |> toFullResult program.mainFile.maybeLocation
        |> bind (fun _ -> validateFileArray program.secondaryFiles)
        |> map (fun _ -> program)