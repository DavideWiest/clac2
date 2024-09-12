module rec Clac2.DomainUtilities 

open Clac2.Domain

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
    else Error ("Function not found: " + f)
