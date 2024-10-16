module rec Clac2.Interpreter

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result
open Clac2.DomainUtilities

let evaluateFile (stdCtx: StandardContext) (file: MainFile) : FullClacResult<DefinedValue> array =
    let dummyLoc = { fileLocation = None; lineLocation = 0 }
    let evalCtx = EvalCtx.init stdCtx file.content dummyLoc

    file.content.expressions
    |> Array.map (fun freeManip -> evaluateOne evalCtx freeManip.loc freeManip.manipulation)

let evaluateOne evalCtx loc manipulation  =
    let newEvalCtx = { evalCtx with currentLoc = loc }
    manipulation
    |> fun m -> m[0], substituteMany newEvalCtx Map.empty m[1..]
    |> fun (startFn, args) -> args |> bind (eval newEvalCtx startFn)

let rec substituteOne evalCtx (substitutions: Map<string, DefinedValue>) x : FullClacResult<DefinedValue> =
    match x with
    | Fn f -> 
        if isPrimitive f then f |> readPrimitive |> DefinedPrimitive |> Ok else

        // substitutions first to override globally defined functions
        if substitutions.ContainsKey f then substitutions[f] |> Ok else

        if evalCtx.customAssignmentMap.ContainsKey f then evaluateOne evalCtx evalCtx.customAssignmentMap[f].loc evalCtx.customAssignmentMap[f].fn else
        if evalCtx.stdFunctionsMap.ContainsKey f then toDefinedFn evalCtx f |> Ok else
        
        FullExcFromEvalCtx ("Function not found: " + f) evalCtx

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
            if args.Length = 0 then f |> readPrimitive |> DefinedPrimitive |> Ok else FullExcFromEvalCtx ("Primitive" + f + "used as function.") evalCtx
        else

        f
        |> getSignature evalCtx
        |> toFullExcFromEvalCtx evalCtx
        |> bind (fun signature -> 
            buildArgs evalCtx args signature 
            |> map (fun args -> signature, args)
        )
        |> bind (fun (signature, args) ->
            // change this for currying
            if args.Length <> signature.Length - 1 then FullExcFromEvalCtx "Incorrect number of arguments" evalCtx else 
            if evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f].DefinedFn args |> toGenericResult |> toFullExcFromEvalCtx evalCtx else
            
            let fn = evalCtx.customAssignmentMap[f]
            let evalCtxNew = { evalCtx with currentLoc = fn.loc }
            let substitutions = Map.ofArray (args |> Array.zip fn.args)
            let substitutedFns = substituteMany evalCtxNew substitutions fn.fn

            if fn.fn.Length = 1 then substitutedFns |> map (fun fns -> fns[0]) else

            substitutedFns
            |> bind (fun x -> eval evalCtxNew fn.fn[0] x[1..])
        )

let toDefinedFn evalCtx f = DefinedFn (evalCtx.stdFunctionsMap[f].name, evalCtx.stdFunctionsMap[f].DefinedFn)

let definedValueFnToReference evalCtx (v: DefinedValue) : FullClacResult<Reference> =
    match v with
    | DefinedPrimitive p -> FullExcFromEvalCtx ("Primitive" + p.ToString() + "used as function.") evalCtx
    | DefinedFn (name, _) -> Fn name |> Ok
