module rec Clac2.Core.Utils

open FSharp.Core.Result

let passAndReturn f a = 
    f a
    a

module Map =
    let merge (m1: Map<'a, 'b>) (m2: Map<'a, 'b>) =
        m1 |> Map.fold (fun acc k v -> Map.add k v acc) m2

module Tuple = 
    let applyUnpacked f (i, x) = f i x

module String =
    let cutOffAt (s: string) (subStr: string) = if s.Contains subStr then s.Substring(0, s.IndexOf subStr) else s

module Result =
    let combine (results: Result<'a, 'b> seq) =
        results
        |> Seq.fold (fun acc r ->
            match acc with
            | Ok accList ->
                match r with
                | Ok v -> accList @ [v] |> Ok
                | Error e -> Error e
            | Error e -> Error e
        ) (Ok [])

    let combineToArray result = result |> combine |> map Array.ofSeq

    let combineTwo (r1: Result<'a, 'b>) (r2: Result<'c, 'b>) =
        match (r1, r2) with 
        | Ok a, Ok b -> Ok (a,b)
        | Error e, _ -> Error e
        | _, Error e -> Error e

module Array = 
    let chooseHigherOccurenceElements (count: int) (searchSelection: ('b * 'a) array) (source: 'a array) =
        searchSelection
        |> Array.filter (fun elem ->
            let ocurrences = Array.sumBy (fun e2 -> if elem |> snd = e2 then 1 else 0) source
            ocurrences > count
        )