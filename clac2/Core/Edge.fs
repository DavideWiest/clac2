module rec Clac2.Core.Input

open Clac2.Core.Domain
open Clac2.Core.Exc.Exceptions
open System


let getInput (args: string array) =
    if args.Length = 0 then
        printf "\n\n> "
        Interactive (Console.ReadLine())
    else
        File args[0]

let tryReadFileIntermedExc file  =
        tryReadFile file |> Intermediate.toResultWithoutLine

let tryReadFile file  =
    try
        Ok (System.IO.File.ReadAllText file)
    with
    | :? System.IO.FileNotFoundException as e -> Simple.toExcResult (sprintf "File not found: %s" file)
    | e -> Simple.toExcResult (sprintf "Error reading file \"%s\": %s" file (e.ToString()))