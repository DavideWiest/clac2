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
        loadStandardFiles stdCtx
        |> bind (fun (stdFiles: File array) -> 
            unparsedMainFile
            |> loadMainFile stdCtx
            |> map (fun parsedMainFile  -> parsedMainFile, stdFiles)
        )
        |> bind (fun (mainFile, stdFiles) -> 
            mainFile
            |> loadOtherFiles stdCtx
            |> map (fun otherFiles -> mainFile, otherFiles, stdFiles)
        )
        |> map (fun (mainFile, otherFiles, stdFiles) -> 
            { mainFile = mainFile; otherLocalFiles = otherFiles; standardFiles = stdFiles }
        )

    let loadStandardFiles stdCtx : FullClacResult<File array> =
        Files.standardFileLocations 
        |> Array.map (fun fileLoc ->
            fileLoc
            |> tryReadFile
            |> bind (parse (Some fileLoc) stdCtx)
            |> map (fun lines -> { location = fileLoc; lines = lines })
            |> toFullGenericExc (Some fileLoc)
        )
        |> combineResultsToArray

    let loadOtherFiles stdCtx (mainFile: MainFile) : FullClacResult<File array> =
        match mainFile.maybeLocation with
        | None -> Ok [||]
        | Some fileLoc ->
            findOtherClacFiles mainFile.lines fileLoc
            |> Array.map (fun fileLoc -> 
                fileLoc
                |> tryReadFile
                |> bind (parse (Some fileLoc) stdCtx)
                |> map (fun lines -> { location = fileLoc; lines = lines })
                |> toFullGenericExc (Some fileLoc)
            )
            |> combineResultsToArray

    let findOtherClacFiles lines fileLoc =
        let dir = System.IO.Path.GetDirectoryName fileLoc
        let references = lines |> Array.choose (fun x -> match x with | ModuleReference s -> Some s | _ -> None)

        let extensionValid (path: string) =Files.officialExtensions |> Array.contains (System.IO.Path.GetExtension path)
        let fileIsReferenced (path: string) = references |> Array.contains (System.IO.Path.GetExtension path)
        
        System.IO.Directory.GetFiles dir
        |> Array.filter (fun x -> extensionValid x && x <> fileLoc && fileIsReferenced x)

    let loadMainFile stdCtx mainFile : FullClacResult<MainFile> =
        match mainFile with
        | Interactive s -> 
            s
            |> parse None stdCtx
            |> map (fun lines -> { maybeLocation = None; lines = lines })
            |> toFullGenericExc None
        | File file -> 
            file
            |> tryReadFile
            |> bind (parse (Some file) stdCtx)
            |> map (fun lines -> { maybeLocation = Some file; lines = lines })
            |> toFullGenericExc (Some file)

    let tryReadFile file : ClacResult<string> =
        try
            Ok (System.IO.File.ReadAllText file)
        with
        | :? System.IO.FileNotFoundException as e -> ClacError (sprintf "File not found: %s" file)
        | e -> ClacError (sprintf "Error reading file \"%s\": %s" file (e.ToString()))

module TypeChecking = 

    let checkTypes stdCtx (program: Program) : FullClacResult<Program> =
        let validateFileArray (fileArr: File array) =
            fileArr
            |> Array.map (fun file -> file.location, file.lines)
            |> Array.map (fun (loc, lines) -> Some loc, lines |> TypeChecking.validateTypes stdCtx)
            |> Array.map tupledToFullGenericExc
            |> combineResultsToArray
        
        program.mainFile.lines
        |> (TypeChecking.validateTypes stdCtx)
        |> toFullGenericExc program.mainFile.maybeLocation
        |> bind (fun _ -> validateFileArray program.otherLocalFiles)
        |> bind (fun _ -> validateFileArray program.standardFiles)
        |> map (fun _ -> program)