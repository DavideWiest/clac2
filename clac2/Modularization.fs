module rec Clac2.Modularization

open Clac2.Domain
open Clac2.Utilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open FSharp.Core.Result
open Clac2.Language

module FileLoading =

    let loadAndParseFiles stdCtx (unparsedMainFile: MainFileUnparsed) =
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

    let loadStandardFiles stdCtx : Result<File array, string> =
        Files.standardFileLocations 
        |> Array.map (fun fileLoc ->
            fileLoc
            |> tryReadFile
            |> bind (parse stdCtx)
            |> map (fun lines -> { location = fileLoc; lines = lines })
        )
        |> combineResultsToArray

    let loadOtherFiles stdCtx (mainFile: MainFile) : Result<File array, string> =
        match mainFile.maybeLocation with
        | None -> Ok [||]
        | Some fileLoc ->
            findOtherClacFiles mainFile.lines fileLoc
            |> Array.map (fun fileLoc -> 
                fileLoc 
                |> tryReadFile
                |> bind (parse stdCtx)
                |> map (fun lines -> { location = fileLoc; lines = lines })
            )
            |> combineResultsToArray

    let findOtherClacFiles lines fileLoc =
        let dir = System.IO.Path.GetDirectoryName fileLoc
        let references = lines |> Array.choose (fun x -> match x with | ModuleReference s -> Some s | _ -> None)

        let extensionValid (path: string) =Files.officialExtensions |> Array.contains (System.IO.Path.GetExtension path)
        let fileIsReferenced (path: string) = references |> Array.contains (System.IO.Path.GetExtension path)
        
        System.IO.Directory.GetFiles dir
        |> Array.filter (fun x -> extensionValid x && x <> fileLoc && fileIsReferenced x)

    let loadMainFile stdCtx mainFile =
        match mainFile with
        | Interactive s -> 
            s
            |> parse stdCtx
            |> map (fun lines -> { maybeLocation = None; lines = lines })
        | File file -> 
            file
            |> tryReadFile
            |> bind (parse stdCtx)
            |> map (fun lines -> { maybeLocation = Some file; lines = lines })

    let tryReadFile file =
        try
            Ok (System.IO.File.ReadAllText file)
        with
        | :? System.IO.FileNotFoundException as e -> Error (sprintf "File not found: %s" file)
        | e -> Error (sprintf "Error reading file: %s" file)

module TypeChecking = 

    let checkTypes stdCtx (program: Program) : Result<Program, string> =
        program.mainFile.lines
        |> (TypeChecking.validateTypes stdCtx)
        |> bind (fun _ -> 
            program.otherLocalFiles
            |> Array.map (fun file -> file.lines)
            |> Array.map (TypeChecking.validateTypes stdCtx)
            |> combineResultsToArray
        )
        |> bind (fun _ -> 
            program.standardFiles
            |> Array.map (fun file -> file.lines)
            |> Array.map (TypeChecking.validateTypes stdCtx)
            |> combineResultsToArray
        )
        |> map (fun _ -> program)