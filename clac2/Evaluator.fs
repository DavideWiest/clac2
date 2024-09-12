module rec Clac2.Evaluator

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result
open Clac2.DomainUtilities

let evaluateLines (stdCtx: StandardContext) (lines: Line array) =
    let evalCtx = EvalCtx.init stdCtx lines

    lines 
    |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)
    |> Array.map (evaluateOne evalCtx)

let evaluateOne evalCtx manipulation  =
    manipulation
    |> fun m -> m[0], substituteMany evalCtx Map.empty m[1..]
    |> fun (startFn, args) -> args |> bind (eval evalCtx startFn)

let rec substituteOne evalCtx (substitutions: Map<string, DefinedValue>) x =
    match x with
    | Fn f -> 
        if isPrimitive f then f |> readPrimitive |> DefinedPrimitive |> Ok else
        //printfn "f: %s" f
        //printfn "sub: %A" substitutions
        //printfn "contains: %b" (substitutions.ContainsKey f)
        if substitutions.ContainsKey f then substitutions[f] |> Ok else

        if evalCtx.customAssignmentMap.ContainsKey f then evaluateOne evalCtx evalCtx.customAssignmentMap[f].fn else
        if evalCtx.stdFunctionsMap.ContainsKey f then toDefinedFn evalCtx f |> Ok else
        
        Error ("Function not found: " + f)

let substituteMany evalCtx (substitutions: Map<string, DefinedValue>) m = 
    m |> Array.map (substituteOne evalCtx substitutions) |> combineResultsToArray

let rec eval evalCtx (startFn: Reference) (args: DefinedValue array) =
    let buildArgs (argsBefore: DefinedValue array) (signature: FnType array) =
        if argsBefore.Length = signature.Length - 1 then argsBefore |> Ok else 
        
        argsBefore[signature.Length - 2]
        |> definedValueFnToReference
        |> bind (fun startFn -> eval evalCtx (startFn) argsBefore[signature.Length - 1..])
        |> map (fun lastArg -> argsBefore[0..signature.Length - 3] |> Array.append [| lastArg |])

    match startFn with
    | Fn f -> 

        if isPrimitive f then 
            if args.Length = 0 then f |> readPrimitive |> DefinedPrimitive |> Ok else Error "Primitive used as function."
        else

        f
        |> getSignature evalCtx
        |> bind (fun signature -> 
            buildArgs args signature 
            |> map (fun args -> signature, args)
        )
        |> bind (fun (signature, args) ->
            if args.Length <> signature.Length - 1 then Error "Incorrect number of arguments" else 
            if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].DefinedFn args else // error here: args have to be evaluated
            
            let fn = evalCtx.customAssignmentMap[f]
            let substitutions = Map.ofArray (args |> Array.zip fn.args)

            let substitutedFns = substituteMany evalCtx substitutions fn.fn

            if fn.fn.Length = 1 then substitutedFns |> map (fun fns -> fns[0]) else

            substitutedFns
            |> bind (fun x -> eval evalCtx fn.fn[0] x[1..])
        )

let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)

let definedValueFnToReference (v: DefinedValue) =
    match v with
    | DefinedPrimitive p -> Error "Primitive used as function."
    | DefinedFn (name, _) -> Fn name |> Ok
