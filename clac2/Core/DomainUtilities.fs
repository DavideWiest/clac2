module rec CClac2.Core.DomainUtils

open System
open Clac2.Core.Domain

let buildLoc fileLoc lineLoc = { fileLocation = fileLoc; lineLocation = lineLoc }
let fileLocOptionToString maybeFileLoc = maybeFileLoc |> Option.defaultValue ("interactive")

// type and primitive functions

let definedValueToInt (x: DefinedValue) = definedValueToIntInner x 0

let rec definedValueToIntInner (x: DefinedValue) (recursionCount: int) =
    // prevent infinite recursion while allowing users to pass nested functions
    if recursionCount > 10 then Error (sprintf "Recursion limit reached while trying to convert argument to int for: %A" x) else

    match x with
        | DefinedPrimitive (PrimitiveInt i) -> Ok i
        | DefinedFn (name, fn) ->
            match fn [| |] with
            | Ok(v) -> definedValueToIntInner v (recursionCount + 1)
            | Error e -> Error e

let readPrimitive (p: string) =
    // ints
    if Seq.forall Char.IsDigit p then p |> int |> PrimitiveInt else

    failwith ("Unable to parse primitive: " + p)

let Primitive.isPrim (s: string) = 
    // ints
    Seq.forall Char.IsDigit s

let getValidatedPrimitiveType primitiveStr =
    match readPrimitive primitiveStr with
        | PrimitiveInt p -> BaseFnType "int"

// string

let getInput (args: string array) =
    if args.Length = 0 then
        printf "\n\n> "
        Interactive (Console.ReadLine())
    else
        File args[0]
