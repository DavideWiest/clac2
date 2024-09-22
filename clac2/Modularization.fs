module rec Clac2.Modularization

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open FSharp.Core.Result
open Clac2.Language

module FileLoading =

    let loadAndParseFiles stdCtx (unparsedMainFile: MainFileUnparsed) : FullClacResult<Program> =
        loadOtherFiles stdCtx Files.standardFileLocations
        |> bind (fun (stdFiles: File array) -> 
            unparsedMainFile
            |> loadMainFile stdCtx
            |> map (fun parsedMainFile  -> parsedMainFile, stdFiles)
        )
        |> bind (fun (mainFile, stdFiles) -> 
            mainFile.maybeLocation
            |> Option.map (findOtherClacFiles mainFile.content)
            |> Option.defaultValue [||]
            |> loadOtherFiles stdCtx 
            |> map (fun otherFiles -> mainFile, otherFiles, stdFiles)
        )
        |> map (fun (mainFile, otherFiles, stdFiles) -> 
            { mainFile = mainFile; secondaryFiles = Array.concat [otherFiles; stdFiles] }
        )

    let loadOtherFiles stdCtx fileLocs : FullClacResult<File array> = loadAllFilesInner stdCtx fileLocs

    let findOtherClacFiles lines fileLoc =
        let dir = System.IO.Path.GetDirectoryName fileLoc
        let references = lines.moduleReferences |> Array.map (fun x -> Files.toQualifiedFileLoc dir x)

        let fileIsReferenced (path: string) = references |> Array.contains (System.IO.Path.GetExtension path)
        
        System.IO.Directory.GetFiles dir
        |> Array.filter (fun x -> x <> fileLoc && fileIsReferenced x)

    let loadMainFile stdCtx mainFile : FullClacResult<MainFile> =
        match mainFile with
        // find a workaround for interactive main files
        | Interactive s -> 
        | File file -> 

    // work with mailFileLoc
    let rec loadAllFilesInner stdCtx (mailFileLoc: string) : FullClacResult<File array> =
        let determineDependencies (currentDir: string) preParsedLines =
            preParsedLines
            |> Array.choose (fun (i, line) -> match line with | UnparsedModuleReference s -> Some s | _ -> None)
            |> Array.map (Files.toQualifiedFileLoc currentDir)
        
        let preparseFile (fileLoc: string) =
            fileLoc
            |> tryReadFileIntermedExc
            |> bind (preparse stdCtx)
            |> toFullResult (Some fileLoc)

        let getDir (fileLoc: string) = getDirOfReference (Some fileLoc)
        
        let buildDepMap filesPreParsedLines =
            filesPreParsedLines
            |> Array.map (fun (fileLoc, preParsedLines) -> fileLoc, determineDependencies (getDir fileLoc) preParsedLines)
            |> Map.ofArray

        let rec loadDefCtxFromDependencies (depMap: Map<string, string array>) (fileContentsMap: Map<string, (int * UnparsedLine) array>) (depMemo: Map<string, DefinitionContext>) fileLoc : DefinitionContext * Map<string, DefinitionContext> =
            if depMemo.ContainsKey fileLoc then (depMemo.[fileLoc], depMemo) else

            let localDefinedSymbols = extractCustomDefinitions fileContentsMap[fileLoc]
            let dependencies = depMap[fileLoc]

            Array.fold (fun acc dep -> 
                let (accDefCtx, depMemo) = acc
                let (depDefCtx, depMemoNew) = loadDefCtxFromDependencies depMap fileContentsMap depMemo dep
                let depMemoNew2 = Map.add dep depDefCtx depMemoNew
                {
                    types = Array.concat [accDefCtx.types; depDefCtx.types]; 
                    functions = Array.concat [accDefCtx.functions; depDefCtx.functions]
                }, depMemoNew2
            ) (localDefinedSymbols, depMemo) dependencies

        let filesPreParsedLines = 
            fileLocs
            |> Array.map preparseFile
            |> combineResultsToArray
            |> map (Array.zip fileLocs)

        filesPreParsedLines
        |> map (fun filesPreParsedLines ->
            let depMap = buildDepMap filesPreParsedLines
            let fileContentsMap = filesPreParsedLines |> Map.ofArray

            filesPreParsedLines
            |> Array.map (fun (fileLoc, preParsedLines) -> 
                let (defCtx, _) = loadDefCtxFromDependencies depMap fileContentsMap Map.empty fileLoc
                fileLoc, preParsedLines, defCtx
            )
        )
        |> bind (fun filesTuple ->
            filesTuple
            |> Array.map (fun (fileLoc, preParsedLines, defCtx) ->
                parseFullResult (Some fileLoc) { stdCtx with defCtx = defCtx} preParsedLines
                |> map (fun orderedFile ->
                    { location = fileLoc; content = orderedFile }
                )
            )
            |> combineResultsToArray
        )
        
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