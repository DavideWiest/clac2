module rec Clac2.Core.Edge

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
        tryReadFile file |> toIntermediateResultWithoutLine

let tryReadFile file  =
    try
        Ok (System.IO.File.ReadAllText file)
    with
    | :? System.IO.FileNotFoundException as e -> GenExcError (sprintf "File not found: %s" file)
    | e -> GenExcError (sprintf "Error reading file \"%s\": %s" file (e.ToString()))