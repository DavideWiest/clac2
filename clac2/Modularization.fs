module rec Clac2.Modularization

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Exc.Domain
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Edge
open Clac2.Core.Representation
open Clac2.Core.Language
open Clac2.FrontEnd.Domain
open Clac2.FrontEnd.FrontEnd

type fileContentsMemo = Map<string option, (int * UnparsedLine) array>


let loadAndParseFiles (stdCtx: StandardContext) (unparsedMainFile: MainFileUnparsed) : FullClacResult<Program * depMap> =
    match unparsedMainFile with
    | Interactive s -> loadAllFilesInner stdCtx None s
    | File f -> f |> tryReadFileIntermedExc |> toFullResult (Some f) |> bind (loadAllFilesInner stdCtx (Some f))

let loadAllFilesInner stdCtx (mainFileLoc: string option) (mainFileLines: string) : FullClacResult<Program * depMap> =
    mainFileLines 
    |> preparse
    |> toFullResult mainFileLoc
    |> bind (fun lines -> 
        let startFileContentsMemo = [(mainFileLoc, lines)] |> Map.ofList
        loadDefCtxFromDependencies stdCtx.defCtx startFileContentsMemo Map.empty Files.standardFileLocations [] mainFileLoc
        |> map (fun v -> lines, v)
    )
    |> bind (fun (mainFileLines, dependencyResult) ->
        let (mainFileDefCtx, depMap, fileContentsMemo) = dependencyResult

        // use mainFileDefCtx, not over depmap, as it does not contain the first one (the main file)
        let mainFileDefCtx' = DefCtx.mergeDefCtxFromStdCtx stdCtx mainFileDefCtx
        let maybeMainFile = 
            parseFullResult mainFileLoc mainFileDefCtx' mainFileLines
            |> map (fun orderedFile -> { maybeLocation = mainFileLoc; content = orderedFile })

        let maybeOtherFiles = 
            fileContentsMemo
            |> Map.toArray
            |> Array.filter (fun (loc, _) -> loc.IsSome && loc <> mainFileLoc)
            |> Array.map (fun (k,v) -> k.Value, v)
            |> Array.map (fun (fileLoc, preParsedLines) ->
                parseFullResult (Some fileLoc) (DefCtx.getDefCtxWithStdCtxFromMap stdCtx depMap (Some fileLoc)) preParsedLines
                |> map (fun orderedFile -> { location = fileLoc; content = orderedFile })
            )
            |> combineResultsToArray

        joinTwoResults maybeMainFile maybeOtherFiles
        |> map (fun (mainFile, otherFiles) -> { mainFile = mainFile; secondaryFiles = otherFiles })
        |> map (fun program -> program, depMap)
    )

let rec loadDefCtxFromDependencies (baseDefCtx: DefinitionContext) (fileContentsMemo: fileContentsMemo) (depMemo: depMap) (baseDeps: string array) (filesHigherUp: string option list) (maybeFileLoc: string option) : FullClacResult<DefinitionContext * depMap * fileContentsMemo> =
    let buildInnerException e = e |> Error |> toGenericResult |> toIntermediateResultWithoutLine |> toFullResult maybeFileLoc
    
    if List.contains maybeFileLoc filesHigherUp then buildInnerException ("Circular import of " + fileLocOptionToString maybeFileLoc) else
    if depMemo.ContainsKey maybeFileLoc then (depMemo[maybeFileLoc], depMemo, fileContentsMemo) |> Ok else

    match fileContentsMemo.TryFind maybeFileLoc with
    | Some content -> Ok content
    | None -> 
        maybeFileLoc
        |> Option.map (preparseFile)
        |> Option.defaultValue (buildInnerException "Internal error: FileContentsMap does not contain interactive file")
    |> bind (fun content -> 
        let localDefinedSymbols = extractCustomDefinitions content
        let dependencies = 
            Array.concat [
                // base deps are not imported if the file is a base dep itself
                (if maybeFileLoc.IsNone || (Array.contains maybeFileLoc.Value baseDeps |> not) then baseDeps else [||])
                // declared dependencies
                determineDependencies (getDirOfReference maybeFileLoc) content
            ] 
            |> Array.distinct

        let fileContentsMemoWithThisFile = Map.add maybeFileLoc content fileContentsMemo

        Array.fold (fun acc dep -> 
            acc
            |> bind (fun (accDefCtx, depMemo, fileContentsMemo) ->
                // non-main files can not be None - this is crucial
                loadDefCtxFromDependencies baseDefCtx fileContentsMemo depMemo baseDeps (maybeFileLoc::filesHigherUp) (Some dep)
                |> bind (fun (depDefCtx, depMemoNew, fileContentsMemoNew) ->
                    let depMemoNew2 = Map.add (Some dep) depDefCtx depMemoNew

                    Ok ({
                        types = Array.append accDefCtx.types depDefCtx.types; 
                        functions = Array.append accDefCtx.functions depDefCtx.functions
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