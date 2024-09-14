module rec Clac2.Utilities

open Clac2.Domain
open System
open FSharp.Core.Result

// Map

module Map =
    let merge (m1: Map<'a, 'b>) (m2: Map<'a, 'b>) =
        m1 |> Map.fold (fun acc k v -> Map.add k v acc) m2

let getKeys (m: Map<'a, 'b>) =
    m |> Map.toSeq |> Seq.map fst |> Seq.toArray

let getValues (m: Map<'a, 'b>) =
    m |> Map.toSeq |> Seq.map snd |> Seq.toArray

// String

let getInput (args: string array) =
    if args.Length = 0 then
        printf "Enter the program:\n"
        Interactive (Console.ReadLine())
    else
        File args[0]

let stringIsEmpty (s: string) =
    s.Trim().Length = 0

let trimSplit (cs: char array) (s: string) =
    s.Split cs
    |> Array.map (fun x -> x.Trim())

// Results

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

let combineResultsToArray result = result |> combineResults |> map Array.ofList

let joinErrorTuple (results: Result<'a, string> * Result<'b, string>) = 
    match results with
    | Error e, Ok _ -> e
    | Ok _, Error e -> e
    | Error e1, Error e2 ->(e1 + "\n" + e2)
    | _ -> "Misused joinErrorTuple: both results are Ok"

// Array

let hasDuplicatesBy (arr: 'a array) (f: 'a -> 'b) =
    arr |> Array.groupBy f |> Array.exists (fun (_, a) -> a.Length > 1)

let hasDuplicatesByReturning (arr: 'a array) (f: 'a -> 'b) =
    arr |> Array.groupBy f |> Array.filter (fun (_, a) -> a.Length > 1) |> Array.map fst

// Misc base

let resultToReturnCode x =
    match x with 
    | Ok _ -> 0 
    | Error _ -> 1

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

// Primitives

let rec definedValueToPrimitive (x: DefinedValue) =
    match x with
        | DefinedPrimitive (PrimitiveInt i) -> Ok i
        | DefinedFn (_, fn) ->
            match fn [| |] with
            | Ok(v) -> definedValueToPrimitive v
            | Error e -> Error e
            | _ -> Error ("Internal Error: not an int as argument of fnTypeToIntAdapter: " + x.ToString())

let readPrimitive (p: string) =
    if Seq.forall Char.IsDigit p then p |> int |> PrimitiveInt else

    failwith ("Unable to parse primitive: " + p)

let isPrimitive (s: string) = 
    Seq.forall Char.IsDigit s

let getPrimitiveType primitiveStr =
    match readPrimitive primitiveStr with
        | PrimitiveInt p -> BaseFnType "int"
