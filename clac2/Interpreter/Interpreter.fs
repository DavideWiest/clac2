module rec Clac2.Interpreter.Interpreter

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Interpreter.EvalCtx
open Clac2.Core.Lang.Primitive

let evaluateFile definedCtx program =
    program.mainFile.content.expressions |> Array.map (fun freeManip -> evaluateOne (EvalCtx.init definedCtx program) freeManip.loc freeManip.manip Map.empty)

let evaluateOne oldEvalCtx loc manipulation substitutions  =
    let evalCtx = { oldEvalCtx with locTrace = loc :: oldEvalCtx.locTrace }

    if manipulation.Length = 1 then substituteOne evalCtx substitutions [||]  manipulation[0] else

    manipulation[1..] 
    |> Array.map (substituteOne evalCtx substitutions [||]) 
    |> Result.combineToArray
    |> bind (fun tail ->
        match manipulation[0] with
        | Fn f when substitutions.ContainsKey f -> PrimitiveOrApply evalCtx tail substitutions[f]
        | _ -> substituteOne evalCtx Map.empty tail manipulation[0]
    )

let rec substituteOne evalCtx substitutions args x =
    let applicationDefined fn = PrimitiveOrApply evalCtx args fn
    let applicationCustom (fn: CallableFunction) = evaluateOne evalCtx fn.loc fn.manip (args |> Array.zip fn.args[..args.Length-1] |> Map.ofArray) // in case of curried functions, right?

    match x with
    | Fn f -> toDefinedOrApply evalCtx substitutions f applicationDefined applicationCustom
    | Manipulation m -> evaluateOne evalCtx (EvalCtx.getCurrentLoc evalCtx) m substitutions |> bind (PrimitiveOrApply evalCtx args)

let toDefinedOrApply evalCtx substitutions f applicationDefined applicationCustom =
    let maybePrim = Primitive.read f
    if maybePrim.IsSome then maybePrim.Value |> DefinedPrimitive |> Ok
    elif substitutions.ContainsKey f then substitutions[f] |> Ok
    elif evalCtx.stdFunctionsMap.ContainsKey f then evalCtx.stdFunctionsMap[f] |> applicationDefined
    elif evalCtx.customAssignmentMap.ContainsKey f then evalCtx.customAssignmentMap[f] |> applicationCustom else
    EvalCtx.FullExcFromEvalCtx ("Function not found (at evaluation): " + f) evalCtx

let PrimitiveOrApply evalCtx args x =
    match x with
    | DefinedPrimitive p -> DefinedPrimitive p |> Ok
    | DefinedFn (_, fn) -> args |> fn |> EvalCtx.toFullExcFromEvalCtx evalCtx
