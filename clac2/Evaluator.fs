module rec Clac2.Evaluator

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result

let evaluate (stdCtx: StandardContext) (lines: Line array) : Result<DefinedValue, string> array  =
    let customAssignments = lines |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
    let manipulations = lines |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)

    let assignmentMap = customAssignments |> Array.map (fun x -> x.name, x) |> Map.ofArray
    let stdFunctionsMap = stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x) |> Map.ofArray

    manipulations
    |> Array.map (evaluateManipulation stdFunctionsMap assignmentMap)

let evaluateManipulation stdFunctionsMap assignmentMap manipulation : Result<DefinedValue, string> array  =

    manipulation
    |> fun m -> m[0], substituteMany stdFunctionsMap assignmentMap Map.empty m[1..]
    |> fun (startFn, args) -> args |> bind (eval stdFunctionsMap assignmentMap startFn)
    |> combineResultsToArray

let rec substituteOne stdFunctionsMap (assignmentMap: Map<string, CallableFunction>) (substitutions: Map<string, DefinedValue>) x =
    match x with
    | Primitive p -> p |> DefinedPrimitive |> Ok
    | Fn f -> 
        if substitutions.ContainsKey f then substitutions[f] |> Ok else
        // functions that are not built-in and are not arguments
        if assignmentMap.ContainsKey f then evaluateManipulation stdFunctionsMap assignmentMap assignmentMap[f]
        else
        if stdFunctionsMap.ContainsKey f then toDefinedFn stdFunctionsMap f |> Ok else failwith ("Function not found: " + f)

let substituteMany stdFunctionsMap assignmentMap (substitutions: Map<string, DefinedValue>) m : Result<DefinedValue array, string> = 
    m 
    |> Array.map (substituteOne stdFunctionsMap assignmentMap substitutions)
    |> combineResultsToArray

let rec eval stdFunctionsMap assignmentMap (startFn: Reference) (args: DefinedValue array) : Result<DefinedValue, string> =
    let buildArgs (argsBefore: DefinedValue array) (signature: FnType array) =
        if argsBefore.Length = signature.Length - 1 then argsBefore |> Ok else 
        
        let args' = argsBefore[0..signature.Length - 3]
        let lastArg = eval stdFunctionsMap assignmentMap (definedValueToReference argsBefore[signature.Length - 2]) argsBefore[signature.Length - 1..]

        match lastArg with
        | Error e -> Error e
        | Ok lastArg -> args' |> Array.append [| lastArg |] |> Ok

    match startFn with
    | Primitive p -> p |> DefinedPrimitive |> Ok
    | Fn f -> 
        printfn "Evaluating %s" f

        let signature = if stdFunctionsMap.ContainsKey f then stdFunctionsMap[f].signature else assignmentMap[f].signature

        match buildArgs args signature with 
        | Error e -> Error e  
        | Ok args ->
            if args.Length <> signature.Length - 1 then Error "Incorrect number of arguments" else 
            if stdFunctionsMap.ContainsKey f then stdFunctionsMap[f].DefinedFn args else
            
            let fn = assignmentMap[f]
            let substitutions = Map.ofArray (args |> Array.zip fn.args)

            // this shouldnt be necessary
            if fn.fn.Length = 1 then substituteOne stdFunctionsMap assignmentMap substitutions fn.fn[0] else

            (substituteMany stdFunctionsMap assignmentMap substitutions fn.fn[1..])
            |> bind (fun x -> eval stdFunctionsMap assignmentMap fn.fn[0] x)

let toDefinedFn stdFunctionsMap f = DefinedFn (stdFunctionsMap[f].name, stdFunctionsMap[f].DefinedFn)

let definedValueToReference (v: DefinedValue) =
    match v with
    | DefinedPrimitive p -> Primitive p
    | DefinedFn (name, _) -> Fn name



