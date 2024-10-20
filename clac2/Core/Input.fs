module rec Clac2.Core.Input

open System
open Clac2.Core.Domain
open Clac2.Core.Exc.Exceptions

let getInput (args: string array) =
    if args.Length = 0 then
        printf "\n\n> "
        Interactive (Console.ReadLine())
    else
        File args[0]

let tryReadFileIntermedExc file  =
        tryReadFile file |> Simple.toResult |> Intermediate.toResultWithoutLine

let tryReadFile file : Result<string, string>  =
    try
        Ok (System.IO.File.ReadAllText file)
    with
    | :? System.IO.FileNotFoundException as e -> Error (sprintf "File not found: %s" file)
    | e -> Error (sprintf "Error reading file \"%s\": %s" file (e.ToString()))