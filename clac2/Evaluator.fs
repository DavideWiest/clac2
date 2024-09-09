module rec Clac2.Evaluator

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result

type EvalCtx = {
    customAssignmentMap: Map<string, CallableFunction>
    stdFunctionsMap: Map<string, DefinedCallableFunction>
}

let evaluateAll (stdCtx: StandardContext) (lines: Line array) : Result<DefinedValue, string> array  =
    let customAssignmentMap = 
        lines 
        |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
        |> Array.map (fun x -> x.name, x) |> Map.ofArray
        
    let evalCtx = {
        customAssignmentMap = customAssignmentMap
        stdFunctionsMap = stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x) |> Map.ofArray
    }

    lines 
    |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)
    |> Array.map (evaluateOne evalCtx)

let evaluateOne evalCtx manipulation : Result<DefinedValue, string> array  =
    manipulation
    |> fun m -> m[0], substituteMany stdFunctionsMap assignmentMap Map.empty m[1..]
    |> fun (startFn, args) -> args |> bind (eval stdFunctionsMap assignmentMap startFn)
    |> combineResultsToArray

let rec substituteOne evalCtx (assignmentMap: Map<string, CallableFunction>) (substitutions: Map<string, DefinedValue>) x =
    match x with
    | Primitive p -> p |> DefinedPrimitive |> Ok
    | Fn f -> 
        if substitutions.ContainsKey f then substitutions[f] |> Ok else
        // functions that are not built-in and are not arguments
        if assignmentMap.ContainsKey f then evaluateManipulation stdFunctionsMap assignmentMap assignmentMap[f]
        else
        if stdFunctionsMap.ContainsKey f then toDefinedFn stdFunctionsMap f |> Ok else failwith ("Function not found: " + f)

let substituteMany evalCtx (substitutions: Map<string, DefinedValue>) m : Result<DefinedValue array, string> = 
    m |> Array.map (substituteOne evalCtx substitutions) |> combineResultsToArray

let rec eval evalCtx (startFn: Reference) (args: DefinedValue array) =
    let buildArgs (argsBefore: DefinedValue array) (signature: FnType array) =
        if argsBefore.Length = signature.Length - 1 then argsBefore |> Ok else 
        
        let args' = argsBefore[0..signature.Length - 3]
        evalOne evalCtx (definedValueToReference argsBefore[signature.Length - 2]) argsBefore[signature.Length - 1..]
        |> bind(fun lastArg -> args' |> Array.append [| lastArg |] |> Ok)

    match startFn with
    | Primitive p -> p |> DefinedPrimitive |> Ok
    | Fn f -> 
        let signature = if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].signature else evalCtx.assignmentMap[f].signature

        match buildArgs args signature with 
        | Error e -> Error e  
        | Ok args ->
            if args.Length <> signature.Length - 1 then Error "Incorrect number of arguments" else 
            if stdFunctionsMap.ContainsKey f then stdFunctionsMap[f].DefinedFn args else
            
            let fn = assignmentMap[f]
            let substitutions = Map.ofArray (args |> Array.zip fn.args)

            (substituteMany stdFunctionsMap assignmentMap substitutions fn.fn[1..])
            |> bind (fun x -> eval stdFunctionsMap assignmentMap fn.fn[0] x)

let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)

let definedValueToReference (v: DefinedValue) =
    match v with
    | DefinedPrimitive p -> Primitive p
    | DefinedFn (name, _) -> Fn name



