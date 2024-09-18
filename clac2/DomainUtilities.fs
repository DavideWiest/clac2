module rec Clac2.DomainUtilities 

open Clac2.Domain
open FSharp.Core.Result

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
}

module EvalCtx =
    let init stdCtx lines =        
        {
            customAssignmentMap = 
                lines 
                |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
                |> Array.map (fun x -> x.name, x) |> Map.ofArray
            stdFunctionsMap = 
                stdCtx.definedCtx.functions 
                |> Array.map (fun x -> x.name, x) 
                |> Map.ofArray
        }

let getSignature evalCtx f =
    if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].signature |> Ok else 
    if evalCtx.customAssignmentMap.ContainsKey f then evalCtx.customAssignmentMap[f].signature |> Ok
    else Error (genericExc ("Function not found: " + f))

let genericExc (e: string) = { message = e }

let ClacError (e: string) = e |> genericExc |> Error

let toClacResult (result: Result<'a, string>) : ClacResult<'a> =
    match result with
    | Ok v -> Ok v
    | Error e -> ClacError e

let combineClacResultsToArray (results: ClacResult<'a> seq) : ClacResult<'a array> =
    results
    |> Array.ofSeq
    |> Array.fold (fun acc r ->
        match acc with
        | Ok accList ->
            match r with
            | Ok v -> Ok (Array.append accList [| v |])
            | Error e -> Error e
        | Error e -> Error e
    ) (Ok [| |])

let printClacError e =
    printfn "%s" e.message

let tupledToFullGenericExc (result: string option * ClacResult<'a>) : FullClacResult<'a> = 
    let (location, err) = result
    toFullGenericExc location err

let toFullGenericExc (location: string option) (result: ClacResult<'a>) : FullClacResult<'a> =
    result
    |> mapError (fun e -> fullGenericExc location e)

let fullGenericExc (location: string option) (genericExc: GenericException) =
    { genExc = genericExc; location = location }

let printFullClacError (e: FullGenericException) =
    printfn "Error occured in %s:" (if e.location.IsSome then e.location.Value else "unknown file location (interactive?)")
    printClacError e.genExc 

let FullClacError (e: string) (location: string option) = e |> genericExc |> fullGenericExc location |> Error