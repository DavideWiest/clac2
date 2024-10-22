module rec Clac2.Modularization

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Domain
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Input
open Clac2.Core.Representation
open Clac2.Core.Lang.Language
open Clac2.FrontEnd.Domain
open Clac2.FrontEnd.FrontEnd

type fileContentsMemo = Map<string option, (int * UnparsedLine) array>

let loadAndParseFiles defCtx unparsedMainFile =
    match unparsedMainFile with
    | Interactive s -> loadAllFilesInner defCtx None s
    | File f -> f |> tryReadFile |> mapError (ErrPipe.toFullExcFromParts None (Some f)) |> bind (loadAllFilesInner defCtx (Some f))

let loadAllFilesInner defCtx mainFileLoc mainFileLines =
    mainFileLines 
    |> lexFull
    |> Full.toResult mainFileLoc
    |> bind (fun lines -> 
        let startFileContentsMemo = [(mainFileLoc, lines)] |> Map.ofList
        loadDefCtxFromDependencies defCtx startFileContentsMemo Map.empty Files.standardFileLocations [] mainFileLoc
        |> map (fun v -> lines, v)
    )
    |> bind (fun (mainFileLines, dependencyResult) ->
        let (mainFileDefCtx, depMap, fileContentsMemo) = dependencyResult

        // use mainFileDefCtx, not over depmap, as it does not contain the first one (the main file)
        let mainFileDefCtx' = ScopeCtx.mergeDefCtx defCtx mainFileDefCtx
        let maybeMainFile = 
            parseFullResult mainFileLoc mainFileDefCtx' mainFileLines
            |> map (fun orderedFile -> { maybeLocation = mainFileLoc; content = orderedFile })

        let maybeOtherFiles = 
            fileContentsMemo
            |> Map.toArray
            |> Array.filter (fun (loc, _) -> loc.IsSome && loc <> mainFileLoc)
            |> Array.map (fun (k,v) -> k.Value, v)
            |> Array.map (fun (fileLoc, preParsedLines) ->
                parseFullResult (Some fileLoc) (ScopeCtx.getDefCtxWithStdCtxFromMap defCtx depMap (Some fileLoc)) preParsedLines
                |> map (fun orderedFile -> { location = fileLoc; content = orderedFile })
            )
            |> Result.combineToArray

        Result.combineTwo maybeMainFile maybeOtherFiles
        |> map (fun (mainFile, otherFiles) -> { mainFile = mainFile; secondaryFiles = otherFiles })
        |> map (fun program -> program, depMap)
    )

let rec loadDefCtxFromDependencies baseDefCtx fileContentsMemo depMemo baseDeps filesHigherUp maybeFileLoc : FullResult<ScopeCtx * fileDependencyMap * fileContentsMemo> =
    let buildInnerException e = ErrPipe.toFullExcFromParts None maybeFileLoc e
    
    if List.contains maybeFileLoc filesHigherUp then buildInnerException ("Circular import of " + ToString.fileLocOption maybeFileLoc) |> Error else
    if depMemo.ContainsKey maybeFileLoc then (depMemo[maybeFileLoc], depMemo, fileContentsMemo) |> Ok else

    match fileContentsMemo.TryFind maybeFileLoc with
    | Some content -> Ok content
    | None -> 
        maybeFileLoc
        |> Option.map (preparseFile)
        |> Option.defaultValue (buildInnerException "Internal error: FileContentsMap does not contain interactive file" |> Error)
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

let determineDependencies currentDir preParsedLines =
    preParsedLines
    |> Array.choose (fun (i, line) -> match line with | UnparsedModuleReference s -> Some s | _ -> None)
    |> Array.map (Files.toQualifiedFileLoc currentDir)

let preparseFile fileLoc =
    fileLoc
    |> tryReadFileIntermedExc
    |> bind lexFull
    |> Full.toResult (Some fileLoc)