module rec Clac2.MiddleEnd.TypeCheckingCtx

open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.MiddleEnd.MiddleEndUtils


type TypeCheckingCtx = {
    types: Map<string, TypeDefinition>
    signatures: Map<string, Signature>
}

module TypeCheckingCtx =
    let init stdCtx program file isMainFile =
        let allTypeDefinitions = 
            program.secondaryFiles
            |> Array.map (fun f -> f.content.typeDefinitions)
            |> Array.concat
            |> Array.append file.typeDefinitions 

        let typeMap = 
            allTypeDefinitions
            |> Array.map (fun x -> x.name, x) 
            |> Map.ofArray

        // only secondary files are added, excluding the main file
        let functionSignatureMap = generateFunctionSignatureMap stdCtx.callableCtx program (if isMainFile then Some file else None) // save computation

        {
            types = typeMap
            signatures = functionSignatureMap
        }


type StandardContext = {
    defCtx: DefinitionContext
    callableCtx: DefinedContext
}

module StdCtx =
    let init definitionContext definedContext  =
        {
            defCtx = definitionContext
            callableCtx = definedContext 
        }