
open Clac2.Domain
open Clac2.Utilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open Clac2.Evaluator
open FSharp.Core.Result

let supportedTypes = 
    [|
        "int"
    |]

let baseVars = 
    [
        ("pi", 3)
        ("e", 3)
    ]
    |> List.map (fun (k, v) -> k, v |> PrimitiveInt |> Primitive)
    |> Map.ofList

let baseFuncs = 
    [
        ("plus", fun (xy: int array) -> xy.[0] + xy.[1])
        ("minus", fun xy -> xy.[0] - xy.[1])
        ("times", fun xy -> xy.[0] * xy.[1])
        ("div", fun xy -> xy.[0] / xy.[1])
    ]
    |> List.map (fun (k, v) -> 
        let newFn (xy: DefinedValue array) = fnTypeToIntAdapter xy v 2

        k,
        {
            name = k
            signature = [| BaseFnType "int"; BaseFnType "int" |]
            args = [| "x"; "y" |]
            DefinedFn = [| DefinedFn newFn |]
        }
    )
    |> Map.ofList

[<EntryPoint>]
let main (args) =
    //let input = getInput args
    let input = "plus 5 5;"  //"let id x : int int = x; let y : int = 5; y" // "id y; plus 5 5; plus y 5"

    printfn "Input:\n%s\n" input

    let stdCtx = buildStandardContext baseVars baseFuncs supportedTypes

    input
    |> parse stdCtx.defCtx
    |> (fun x ->
        match x with 
        | Ok lines ->
            printfn "Parsed:\n"
            lines
            |> Array.map lineToString
            |> Array.iter (printfn "%s")

        | Error e ->
            printfn "Error:\n%s" e

        x
    )
    |> bind (checkTypes stdCtx)
    |> bind (evaluate stdCtx)
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results
            |> Array.iter (printfn "%A")

        | Error e ->
            printfn "Error:\n%s" e

        x
    )
    |> resultToReturnCode