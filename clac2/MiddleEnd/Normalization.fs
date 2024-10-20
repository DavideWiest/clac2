module rec Clac2.MiddleEnd.Normalization

open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Lang.Primitive
open Clac2.MiddleEnd.MiddleEndUtils

let applyNormalizationPipeline definedCtx program = normalizationPipeline |> Array.fold (fun acc f -> f definedCtx acc) program

// this order is important
let normalizationPipeline: (DefinedContext -> Program -> Program) array = [|
    ArgumentPropagation.nestArgumentPropagations
    OperatorFixation.operatorFixationCorrection
|]

module ArgumentPropagation = 
    let nestArgumentPropagations definedCtx program =
        let functionSignatureMap = generateFunctionSignatureMap definedCtx program (Some program.mainFile.content)
        mapAllManipulations program (nestArgumentPropagationsInner (createFixationMap definedCtx program) functionSignatureMap)

    let nestArgumentPropagationsInner (fixationMap: Map<string, OperatorFixation>) (functionSignatureMap: Map<string,Signature>) manipParent =
        let fnBuilder manipParent'= 
            match manipParent' with
            | Manip _ -> nestArgumentPropagationsManip fixationMap functionSignatureMap
            | CallableFunction a -> nestArgumentPropagationsManip fixationMap (Signature.addArgsToSignatureMap a functionSignatureMap)

        applyManipulationApplicationFromManipParent manipParent fnBuilder

    // argument propagation is not supported for curried functions either
    let rec nestArgumentPropagationsManip fixationMap functionSignatureMap (m: Manipulation) : Manipulation =
        let propagate (m: Manipulation) sigLen = if m.Length - 1 <= sigLen - 1 then m else Array.append m[..sigLen-2] [| Manipulation m[sigLen-1..] |]

        if m.Length = 1 then m
        // if it is in fixationMap, it is also in functionSignatureMap (the reverse is not true - arguments)
        elif fixationMapLookup fixationMap m[0] = Some Prefix then 
            propagate m (functionSignatureMapLookup functionSignatureMap m[0]).Value.Length
        elif fixationMapLookup fixationMap m[1] = Some Infix then 
            propagate m (functionSignatureMapLookup functionSignatureMap m[1]).Value.Length
        // propagation is not supported for postfix functions and arguments
        else m

module OperatorFixation = 
    let operatorFixationCorrection definedCtx program = mapAllManipulations program (operatorFixationToPrefixOuter (createFixationMap definedCtx program))

    let operatorFixationToPrefixOuter fixationMap manip =
        applyManipulationApplicationFromManipParent manip (fun _ -> operatorFixationToPrefix fixationMap)
    
    let operatorFixationToPrefix fixationMap manipFirst =
        let fixationFixOneManip (manip: Manipulation) =
            match manip with
            | [| _ |] -> manip
            | _ when fixationMapLookup fixationMap manip.[manip.Length - 1] = Some Postfix -> Array.append [| manip.[manip.Length - 1] |] manip.[..manip.Length - 2]
            | [| f; s; t |] when fixationMapLookup fixationMap s = Some Infix -> [| s; f; t |]
            | _ -> manip

        manipFirst |> fixationFixOneManip

type ManipulationWrapper = Manip of Manipulation | CallableFunction of CallableFunction

let mapAllManipulations program manipMapOne = 
    let mapExpression (e: FreeManipulation) =  { e with manip = manipMapOne (Manip e.manip) }
    let mapAssignment (a: CallableFunction) = { a with manip = manipMapOne (CallableFunction a) }
    let mapFile f = { f with expressions = f.expressions |> Array.map mapExpression; assignments = f.assignments |> Array.map mapAssignment }

    let newMainFile = { program.mainFile with content = mapFile program.mainFile.content }
    let newSecondaryFiles = program.secondaryFiles |> Array.map (fun f -> { f with content = mapFile f.content })

    { program with mainFile = newMainFile; secondaryFiles = newSecondaryFiles }

// for the argument propagation normalization, it is crucial to apply the function to the inner manipulations first
// for functions that simplify manipulations (bottom-up), it is crucial to apply the function to the outer manipulations first
let applyManipulationApplicationFromManipParent manipParent fnBuilder =
    let fn = fnBuilder manipParent
    match manipParent with
    | Manip m -> m |> fn |> Array.map (recursivelyApplyToInnerManipulations fn)
    | CallableFunction a -> a.manip |> fn |> Array.map (recursivelyApplyToInnerManipulations fn)

// Note: The recursion has to be inside the function passed to mapAllManipulations, since data has to be used from the ManipulationWrapper
let rec recursivelyApplyToInnerManipulations f x =
    match x with
    | Manipulation m -> m |> f |> Array.map (recursivelyApplyToInnerManipulations f) |> Manipulation // inner first or outer first? It probably doesn't matter
    | Fn _ -> x

let createFixationMap definedCtx program =
    (Array.append (program.secondaryFiles |> Array.map (fun x -> x.content)) [| program.mainFile.content |])
    |> Array.map extractFixationFromFile
    |> Array.concat
    |> Array.append (extractFixationFromBuiltIns definedCtx.functions)
    |> Map.ofArray

let extractFixationFromFile orderedFile = orderedFile.assignments |> Array.map (fun x -> x.name, x.fnOptions.fixation)
let extractFixationFromBuiltIns functions = functions |> Array.map (fun x -> x.name, x.fnOptions.fixation)

// dont try to create a wrapper function for those two - type checking prohibits it

let fixationMapLookup (fixationMap: Map<string,OperatorFixation>) x =
    match x with
    | Fn fn when Primitive.isPrim fn |> not -> if fixationMap.ContainsKey fn then Some fixationMap[fn] else None
    | _ -> None

let functionSignatureMapLookup (functionSignatureMap: Map<string,Signature>) x : Signature option =
    match x with
    | Fn fn when Primitive.isPrim fn |> not -> if functionSignatureMap.ContainsKey fn then Some functionSignatureMap[fn] else None
    | _ -> None