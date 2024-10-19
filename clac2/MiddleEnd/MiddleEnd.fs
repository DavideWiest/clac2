module rec Clac2.MiddleEnd.TypeChecker

open Clac2.Core.Domain
open Clac2.Core.Utils
open Clac2.Core.Exc.Exceptions
open Clac2.Core.DomainUtils
    

module TypeChecking =

    let validateTypes stdCtx program isMainFile file =
        match validateTypesInner stdCtx program isMainFile file with
        | None -> Ok file
        | Some e -> Error e

    // check types, then manipulations, then assignments
    let validateTypesInner stdCtx program isMainFile file = 
        // typeCheckingCtx should be precomputed for all files and passed into here - the file in question is the main one, append the function signature
        let typeCheckingCtx = TypeCheckingCtx.init stdCtx program file isMainFile

        checkTypeDefinitions stdCtx typeCheckingCtx file.typeDefinitions
        |> Option.orElse (checkManipulationsAnyOutputType typeCheckingCtx file.expressions)
        |> Option.orElse (checkAssignments typeCheckingCtx file.assignments) 

    // not necessary to check all types, only custom ones (will run for each file)
    let checkTypeDefinitions (stdCtx: StandardContext) typeCheckingCtx customTypes =    
        // no recursive type definitions - type system is a tree
        let rec checkTypeDefsInner typesHigherUp (x: TypeDefinition) =
            let rec flattenSignature (x: FnType) =
                match x with
                | BaseFnType s -> [| s |]
                | Function fs -> fs |> Array.collect flattenSignature

            let typesHigherUp' = x.name :: typesHigherUp

            x.signature
            |> Array.collect flattenSignature
            |> Array.distinct
            |> Array.tryPick (fun x -> 
                // ignore base types
                if Array.contains x stdCtx.defCtx.types then None else

                let line = getLineForType typeCheckingCtx.types x
                if List.contains x typesHigherUp' then Some(IntermediateExcFPPureMaybeLine ("Recursive type definition: " + x) line) else 
                if Map.containsKey x typeCheckingCtx.types |> not then Some(IntermediateExcFPPureMaybeLine ("Unknown type: " + x) line) else

                checkTypeDefsInner typesHigherUp' typeCheckingCtx.types[x]
            )
         
        Array.tryPick (checkTypeDefsInner []) customTypes

    let checkManipulationsAnyOutputType typeCheckingCtx manipulations =
        manipulations
        |> Array.map (fun x -> x.loc.lineLocation, x.manip)
        |> Array.tryPick (fun (i, x) -> checkManipulation typeCheckingCtx x i AnyOut)

    let checkAssignments typeCheckingCtx customAssignments =
        let checkAssignment (assignment: CallableFunction) =
            let newTypeCheckingCtx = { typeCheckingCtx with signatures = Signature.addArgsToSignatureMap assignment typeCheckingCtx.signatures }
            let expectedOutputType = ExpectedType assignment.signature[assignment.signature.Length-1]
            checkManipulation newTypeCheckingCtx assignment.manip assignment.loc.lineLocation expectedOutputType

        Array.tryPick checkAssignment customAssignments

    type ExcpectedOutputType =
        | AnyOut
        | ExpectedType of FnType

    let rec checkManipulation typeCheckingCtx m line expectedOutputType =        
        let rec typesMatch inputSignature args =
            Array.fold (fun acc (signaturePart, arg) -> 
                let preparedArg = 
                    match arg with
                    | Fn f -> [| Fn f |]
                    | Manipulation m' -> m'

                acc
                |> Option.orElse (checkManipulation typeCheckingCtx preparedArg line (ExpectedType signaturePart))
            ) None (Array.zip inputSignature args)

        if m.Length = 0 then 
            // supports "()"/unit this way
            if expectedOutputType = AnyOut then None else Some (IntermediateExcFPPure ("Got empty manipulation when expecting output type of: " + expectedOutputType.ToString()) line)
        else

        match m[0] with
        | Fn f ->
            if Primitive.isPrim f then
                if m.Length > 1 then Some (IntermediateExcFPPure ("Primitive " + f + " used as function.") line) else

                let primitiveType = getValidatedPrimitiveType f
                if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType primitiveType then Some (IntermediateExcFPPure (sprintf "Expected output type for %s is %A, received %A" f expectedOutputType primitiveType) line) else None
            else

            if not (typeCheckingCtx.signatures.ContainsKey f) then Some (IntermediateExcWithoutLine (GenExc ("Internal Error: customFnsMap does not contain function " + f + ". It probably was not registered in the front end."))) else

            let signature = typeCheckingCtx.signatures[f]
            if m.Length - 1 > signature.Length - 1 then Some (IntermediateExcFPPure (sprintf "Too many arguments for %s: Expected %i, got %i." f (signature.Length-1) (m.Length-1)) line) else

            let inputIStop = m.Length - 2
            let inputSignature, outputSignature = signature[..inputIStop], signature[inputIStop+1..]
            let preparedOutputSignature = if outputSignature.Length = 1 then outputSignature[0] else Function outputSignature

            if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType preparedOutputSignature then Some (IntermediateExcFPPure (sprintf "Expected output type for %s is %A, received %A" f expectedOutputType outputSignature) line) else
            
            typesMatch inputSignature m[1..]
        | Manipulation m' -> 
            // the first manipulation is a curried function, so we can append the first args to the rest
            let newManip = Array.concat [ m' ; m.[1..] ]
            checkManipulation typeCheckingCtx newManip line expectedOutputType

// ----

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
        let functionSignatureMap = generateFunctionSignatureMap stdCtx program (if isMainFile then Some file else None) // save computation

        {
            types = typeMap
            signatures = functionSignatureMap
        }

    let generateFunctionSignatureMap stdCtx program mainFile =
        let typesInFile = if mainFile.IsNone then [||] else mainFile.Value.assignments |> Array.map (fun x -> x.name, x.signature)
        [
            typesInFile
            program.secondaryFiles |> Array.map (fun f -> f.content.assignments |> Array.map (fun x -> x.name, x.signature)) |> Array.concat
            stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature)
        ]
        |> Array.concat
        |> Map.ofArray

let getLineForType customTypes f  = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None

module Signature =
    let addArgsToSignatureMap assignment signatureMap =
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
    
    let getInOutSeparate typeCheckingCtx f =
        let signature = typeCheckingCtx.signatures[f]
        signature[..signature.Length-2], signature[signature.Length-1]

