module rec Clac2.MiddleEnd.MiddleEndUtils

open Clac2.Core.Domain
open Clac2.Core.Utils
open Clac2.MiddleEnd.MiddleEndUtils

let generateFunctionSignatureMap stdCtx program (mainFile: OrderedFile Option) =
    let typesInFile = if mainFile.IsNone then [||] else mainFile.Value.assignments |> Array.map (fun x -> x.name, x.signature)
    [
        typesInFile
        program.secondaryFiles |> Array.map (fun f -> f.content.assignments |> Array.map (fun x -> x.name, x.signature)) |> Array.concat
        stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature)
    ]
    |> Array.concat
    |> Map.ofArray

let getLineForType (customTypes: Map<string, TypeDefinition>) f  = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None

module Signature =
    let addArgsToSignatureMap (assignment: CallableFunction) signatureMap =
        let argumentsAsAssignments = 
            assignment.args 
            |> Array.mapi (fun i x -> 
                (x, Signature.UnpackInnerSignature assignment.signature[i] )
            )
            |> Map.ofArray

        Map.merge argumentsAsAssignments signatureMap

    let UnpackInnerSignature signature =
        match signature with
        | BaseFnType s -> [| BaseFnType s |]
        | Function fs -> fs
    
    let getInOutSeparate (signatureMap: Map<string, Signature>) f =
        let signature = signatureMap[f]
        signature[..signature.Length-2], signature[signature.Length-1]

