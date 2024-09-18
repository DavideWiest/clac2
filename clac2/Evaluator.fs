module rec Clac2.Evaluator

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result
open Clac2.DomainUtilities

let evaluateLines (stdCtx: StandardContext) (lines: Line array) : FullClacResult<DefinedValue> array =
    let evalCtx = EvalCtx.init stdCtx lines

    lines 
    |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)
    |> Array.map (evaluateOne evalCtx)

let evaluateOne evalCtx manipulation  =
    manipulation
    |> fun m -> m[0], substituteMany evalCtx Map.empty m[1..]
    |> fun (startFn, args) -> args |> bind (eval evalCtx startFn)

let rec substituteOne evalCtx (substitutions: Map<string, DefinedValue>) x : FullClacResult<DefinedValue> =
    match x with
    | Fn f -> 
        if isPrimitive f then f |> readPrimitive |> DefinedPrimitive |> Ok else

        if substitutions.ContainsKey f then substitutions[f] |> Ok else

        if evalCtx.customAssignmentMap.ContainsKey f then evaluateOne evalCtx evalCtx.customAssignmentMap[f].fn else
        if evalCtx.stdFunctionsMap.ContainsKey f then toDefinedFn evalCtx f |> Ok else
        
        FullClacError ("Function not found: " + f) evalCtx.currentFile

let substituteMany evalCtx (substitutions: Map<string, DefinedValue>) m : FullClacResult<DefinedValue array> = 
    m |> Array.map (substituteOne evalCtx substitutions) |> combineResultsToArray

let rec eval evalCtx (startFn: Reference) (args: DefinedValue array) : FullClacResult<DefinedValue> =
    let buildArgs evalCtx (argsBefore: DefinedValue array) (signature: FnType array) : FullClacResult<DefinedValue array> =
        if argsBefore.Length = signature.Length - 1 then argsBefore |> Ok else 
        
        argsBefore[signature.Length - 2]
        |> definedValueFnToReference evalCtx
        |> bind (fun startFn -> eval evalCtx (startFn) argsBefore[signature.Length - 1..])
        |> map (fun lastArg -> argsBefore[0..signature.Length - 3] |> Array.append [| lastArg |])

    match startFn with
    | Fn f -> 

        if isPrimitive f then 
            if args.Length = 0 then f |> readPrimitive |> DefinedPrimitive |> Ok else FullClacError "Primitive used as function." evalCtx.currentFile
        else

        f
        |> getSignature evalCtx
        |> toFullGenericExc evalCtx.currentFile
        |> bind (fun signature -> 
            buildArgs evalCtx args signature 
            |> map (fun args -> signature, args)
        )
        |> bind (fun (signature, args) ->
            // change this for currying
            if args.Length <> signature.Length - 1 then FullClacError "Incorrect number of arguments" evalCtx.currentFile else 
            if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].DefinedFn args |> toClacResult |> toFullGenericExc evalCtx.currentFile else
            
            let fn = evalCtx.customAssignmentMap[f]
            let evalCtxNew = { evalCtx with currentFile = fn.fileLocation }
            let substitutions = Map.ofArray (args |> Array.zip fn.args)
            let substitutedFns = substituteMany evalCtxNew substitutions fn.fn

            if fn.fn.Length = 1 then substitutedFns |> map (fun fns -> fns[0]) else

            substitutedFns
            |> bind (fun x -> eval evalCtxNew fn.fn[0] x[1..])
        )

let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)

let definedValueFnToReference evalCtx (v: DefinedValue) : FullClacResult<Reference> =
    match v with
    | DefinedPrimitive p -> ClacError "Primitive used as function." |> toFullGenericExc evalCtx.currentFile
    | DefinedFn (name, _) -> Fn name |> Ok
