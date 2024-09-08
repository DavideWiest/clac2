
open Clac2.Domain
open Clac2.Utilities
open Clac2.FrontEnd
open Clac2.MiddleEnd
open Clac2.Evaluator
open FSharp.Core.Result

let supportedTypes = [| "int" |]
let commentIdentifer = "--"

let intType = BaseFnType "int"
let baseVars = 
    [|
        ("pi", 3)
        ("e", 3)
    |]
    |> Array.map (fun (k, v) -> k, v |> PrimitiveInt |> DefinedPrimitive)
    |> Array.map (FSharpConstantToFn intType)

let basicArithmeticArgsAndSignature = [| ("n1", intType); ("n2", intType) |]

let baseFuncs = 
    [|
        ("plus", (+))
        ("minus", (-))
        ("times", (*))
        ("div", (/))
    |]
    |> Array.map (fun (k, v) -> k, fun input -> Array.reduce v input)
    |> Array.map (fun (k, v) -> k, fun input -> fnTypeToIntAdapter input v 2
    )
    |> Array.map (fSharpFunctionToFn basicArithmeticArgsAndSignature intType)


[<EntryPoint>]
let main (args) =

    //let input = getInput args
    let input' = """
        -- check parsing and assignment syntax validation
        let id x : int int = x
        let y : int = 5
        y
        let w : int = plus 5 5
        plus 5 5
        plus y 5
        plus pi e

        -- check parsing that should fail
        -- type int3 : int3
        -- let abc : = 3
        -- let abc2 : int int = 1
        -- let abc3 a : int int int = 3

        -- check type checking
        type int2 : int
        type intTwice : int int
        type basicArithmetic : int int int
        type basicArithmetic2 : int2 int2 int2

        -- types that should fail
        -- type int5 : int6
        -- type int6 : int5
        -- type int7 : int8

        -- manipulations
        id 5
        plus 5 5
        plus 5 plus 5 5
        e
        let plus3 a b c : int int int int = plus a plus b c

        -- manipulations that should fail
        -- plus id 5
        -- plus id id
    """

    let input = "let y : int = 5; plus y 5"

    let stdCtx = buildStandardContext baseVars baseFuncs supportedTypes commentIdentifer

    input
    |> parse stdCtx
    |> (fun x ->
        match x with 
        | Ok lines ->
            printfn "Parsed:\n"
            lines
            |> Array.map lineToString
            |> Array.iter (printfn "%s")
            printfn "\n---------\n"
        | Error e -> ()

        x
    )
    |> bind (checkTypes stdCtx)
    |> bind (fun lines -> lines |> evaluate stdCtx |> combineResultsToArray)
    |> (fun x ->
        match x with 
        | Ok results ->
            printfn "Evaluated:\n"
            results |> Array.iter (printfn "%A")

        | Error e ->
            printfn "Error:\n%s" e

        x
    )
    |> resultToReturnCode