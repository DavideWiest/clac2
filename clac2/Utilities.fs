module rec Clac2.Utilities

open Clac2.Domain
open System

let getInput (args: string array) =
    let readInput () =
        printf "Enter the program:\n"
        Console.ReadLine()

    let readFile (path: string) =
        System.IO.File.ReadLines path
        |> String.concat "\n"
    
    if args.Length = 0 then
        readInput()
    else
        readFile args.[0]

let stringIsEmpty (s: string) =
    s.Trim().Length = 0

let combineResults (results: Result<'a, 'b> seq) =
    results
    |> Seq.fold (fun acc r ->
        match acc with
        | Ok accList ->
            match r with
            | Ok v -> accList @ [v] |> Ok
            | Error e -> Error e
        | Error e -> Error e
    ) (Ok [])

let combineResultsToArray results =
    results
    |> combineResults
    |> Result.map Array.ofList

let joinErrorTuple (results: Result<'a, string> * Result<'b, string>) = 
    match results with
    | Error e, Ok _ -> e
    | Ok _, Error e -> e
    | Error e1, Error e2 ->(e1 + "\n" + e2)
    | _ -> "Misused joinErrorTuple: both results are Ok"

let trimSplit (cs: char array) (s: string) =
    s.Split cs
    |> Array.map (fun x -> x.Trim())

let getKeys (m: Map<'a, 'b>) =
    m |> Map.toSeq |> Seq.map fst |> Seq.toArray

let hasDuplicatesBy (arr: 'a array) (f: 'a -> 'b) =
    arr |> Array.groupBy f |> Array.exists (fun (_, a) -> a.Length > 1)

let hasDuplicatesByReturning (arr: 'a array) (f: 'a -> 'b) =
    arr |> Array.groupBy f |> Array.filter (fun (_, a) -> a.Length > 1) |> Array.map fst

let lineToString (line: Line) =
    let exprToString m = m |> Array.map string |> String.concat " "

    match line with
    | Expression m -> exprToString m
    | Assignment f -> 
        let args = (f.args |> String.concat " ")
        let signature = (f.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")
        let fnBody = exprToString f.fn

        sprintf "let %s %s : %s = %s" f.name args signature fnBody
    | TypeDefinition t -> 
        let signature = (t.signature |> Array.map (fun x -> x.ToString()) |> String.concat " ")

        sprintf "type %s : %s" t.name signature



let buildStandardContext (baseVars: Map<string, Value>) (baseFuncs: Map<string, DefinedCallableFunction>) (supportedTypes: string array) =
    {
        defCtx = {
            types = supportedTypes
            values = Array.concat [baseVars |> getKeys; baseFuncs |> getKeys]
        }
    }

let resultToReturnCode x =
    match x with 
    | Ok _ -> 0 
    | Error _ -> 1

let fnTypeToIntAdapter (input: DefinedValue array) f nArgs =
    let args =
        input
        |> Array.filter (fun x -> 
            match x with
            | DefinedPrimitive (PrimitiveInt _) -> true
            | _ -> false
        ) 
        |> Array.map (fun x -> 
            match x with
            | DefinedPrimitive (PrimitiveInt i) -> i
            | _ -> failwith "fnTypeToIntAdapter: not an int"
        )

    if args.Length <> nArgs then Error "fnTypeToIntAdapter: wrong number of arguments" else

    f args
    |> PrimitiveInt
    |> DefinedPrimitive
    |> Ok
