module rec Clac2.Core.Utils

open FSharp.Core.Result

// Misc

let passAndReturn f a = 
    f a
    a

// Map

module Map =
    let merge (m1: Map<'a, 'b>) (m2: Map<'a, 'b>) =
        m1 |> Map.fold (fun acc k v -> Map.add k v acc) m2

let getKeys (m: Map<'a, 'b>) =
    m |> Map.toSeq |> Seq.map fst |> Seq.toArray

let getValues (m: Map<'a, 'b>) =
    m |> Map.toSeq |> Seq.map snd |> Seq.toArray

// Tuple

let applyUnpacked f (i, x) = f i x

let applyTupledReversed f (x, i) = f (i, x)

// String

let stringIsEmpty (s: string) =
    s.Trim().Length = 0

let String.cutOffAt (s: string) (subStr: string) = 
    if s.Contains subStr then s.Substring(0, s.IndexOf subStr) else s

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

let Result.combineToArray result = result |> combineResults |> map Array.ofList

let joinTwoResults (r1: Result<'a, 'b>) (r2: Result<'c, 'b>) =
    match (r1, r2) with 
    | Ok a, Ok b -> Ok (a,b)
    | Error e, _ -> Error e
    | _, Error e -> Error e

// Array

let hasDuplicatesBy (arr: 'a array) (f: 'a -> 'b) =
    arr |> Array.groupBy f |> Array.exists (fun (_, a) -> a.Length > 1)

let chooseHigherOccurenceElements (count: int) (searchSelection: ('b * 'a) array) (source: 'a array) =
    searchSelection
    |> Array.filter (fun elem ->
        let ocurrences = Array.sumBy (fun e2 -> if elem |> snd = e2 then 1 else 0) source
        ocurrences > count
    )