module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities

module TypeChecking =

    let validateTypes (stdCtx: StandardContext) (program: Program) (file: OrderedFile) : IntermediateClacResult<OrderedFile> =
        match validateTypesInner stdCtx program file with
        | None -> Ok file
        | Some e -> Error e

    // check types, then manipulations, then assignments
    let validateTypesInner (stdCtx: StandardContext) (program: Program) (file: OrderedFile) : IntermediateException option = 
        // typeCheckingCtx should be precomputed for all files and passed into here - the file in question is the main one, append the function signature
        let typeCheckingCtx = TypeCheckingCtx.init stdCtx program file

        checkTypeDefinitions stdCtx typeCheckingCtx file.typeDefinitions
        |> Option.orElse (checkManipulationsAnyOutputType typeCheckingCtx file.expressions)
        |> Option.orElse (checkAssignments typeCheckingCtx file.assignments) 

    // not necessary to check all types, only custom ones (will run for each file)
    let checkTypeDefinitions (stdCtx: StandardContext) typeCheckingCtx customTypes : IntermediateException option =    
        // no recursive type definitions - type system is a tree
        let rec checkTypeDefsInner (typesHigherUp: string list) (x: TypeDefinition) =
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

    let checkManipulationsAnyOutputType typeCheckingCtx manipulations : IntermediateException option =
        manipulations
        |> Array.map (fun x -> x.loc.lineLocation, x.manipulation)
        |> Array.tryPick (fun (i, x) -> checkManipulation typeCheckingCtx x i AnyOut)

    let checkAssignments typeCheckingCtx customAssignments : IntermediateException option =
        let checkAssignment (assignment: CallableFunction) =
            let argumentsAsAssignments = 
                assignment.args 
                |> Array.mapi (fun i x -> 
                    (x, Signature.UnpackInnerSignature assignment.signature[i] )
                )
                |> Map.ofArray

            let newTypeCheckingCtx = { typeCheckingCtx with signatures = Map.merge argumentsAsAssignments typeCheckingCtx.signatures }
            let expectedOutputType = ExpectedType assignment.signature[assignment.signature.Length-1]
            checkManipulation newTypeCheckingCtx assignment.fn assignment.loc.lineLocation expectedOutputType

        Array.tryPick checkAssignment customAssignments

    type ExcpectedOutputType =
        | AnyOut
        | ExpectedType of FnType

    let rec checkManipulation typeCheckingCtx (m: Reference array) line (expectedOutputType: ExcpectedOutputType) =        
        let rec typesMatch (inputSignature: FnType array) (args: Reference array) : IntermediateException option =
            Array.fold (fun acc (signaturePart: FnType, arg: Reference) -> 
                let preparedArg = 
                    match arg with
                    | Fn f -> [| Fn f |]
                    | Manipulation m' -> m'

                acc
                |> Option.orElse (checkManipulation typeCheckingCtx preparedArg line (ExpectedType signaturePart))
            ) None (Array.zip inputSignature args)

        if m.Length = 0 then 
            if expectedOutputType = AnyOut then None else Some (IntermediateExcFPPure ("Got empty manipulation when expecting output type of: " + expectedOutputType.ToString()) line)
        else

        match m[0] with
        | Fn f ->
            if isPrimitive f then
                if m.Length > 1 then Some (IntermediateExcFPPure ("Primitive " + f + " used as function.") line) else

                let primitiveType = getValidatedPrimitiveType f
                if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType primitiveType then Some (IntermediateExcFPPure (sprintf "Expected output type for %s is %A, received %A" f expectedOutputType primitiveType) line) else None
            else

            if not (typeCheckingCtx.signatures.ContainsKey f) then Some (IntermediateExcWithoutLine (GenExc ("Internal Error: customFnsMap does not contain function " + f + ". It probably was not registered in the front end."))) else

            let signature = typeCheckingCtx.signatures[f]
            let containsArgumentPropagation = m.Length - 1 > signature.Length - 1 

            printfn "m: %A" m
            printfn "signature: %A" signature
            printfn "containsArgumentPropagation: %A" containsArgumentPropagation

            let maybeArgPropagrationError = if containsArgumentPropagation then checkManipulation typeCheckingCtx m[signature.Length - 1..] line (ExpectedType signature[signature.Length - 1]) else None
            if maybeArgPropagrationError.IsSome then maybeArgPropagrationError else

            // if the manipulation contains an argument propagation, the full signature is used
            // if equal or less, the manipulation determines how many parts of the signature are used
            // this might not be right, but i'll leave it for now
            let inputIStop = if containsArgumentPropagation then signature.Length - 2 else m.Length - 2
            let inputSignature, outputSignature = signature[..inputIStop], signature[inputIStop+1..]
            let preparedOutputSignature = if outputSignature.Length = 1 then outputSignature.[0] else Function outputSignature

            if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType preparedOutputSignature then Some (IntermediateExcFPPure (sprintf "Expected output type for %s is %A, received %A" f expectedOutputType outputSignature) line) else
            
            // skip the last argument if it is an argument propagation
            let inputSignatureLeftToCheck = if containsArgumentPropagation then inputSignature[..inputSignature.Length - 2] else inputSignature
            typesMatch inputSignatureLeftToCheck m[1..inputSignatureLeftToCheck.Length]
        | Manipulation m' -> 
            // the first manipulation is a curried function, so we can append the first args to the rest
            let newManip = Array.concat [ m' ; m.[1..] ]
            checkManipulation typeCheckingCtx newManip line expectedOutputType

    module Signature =
        let UnpackInnerSignature signature =
            match signature with
            | BaseFnType s -> [| BaseFnType s |]
            | Function fs -> fs
        
        let getInOutSeparate (typeCheckingCtx: TypeCheckingCtx) f : FnType array * FnType = 
            let signature = typeCheckingCtx.signatures[f]
            signature[..signature.Length-2], signature[signature.Length-1]


    type TypeCheckingCtx = {
        types: Map<string, TypeDefinition>
        signatures: Map<string, Signature>
    }

    module TypeCheckingCtx =
        let init (stdCtx: StandardContext) (program: Program) (file: OrderedFile) : TypeCheckingCtx =
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
            let functionSignatureMap = 
                [
                    file.assignments |> Array.map (fun x -> x.name, x.signature)
                    program.secondaryFiles |> Array.map (fun f -> f.content.assignments |> Array.map (fun x -> x.name, x.signature)) |> Array.concat
                    stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature)
                ] 
                |> Array.concat
                |> Map.ofArray

            printfn "functionSignatureMap: %A" (getKeys functionSignatureMap)

            {
                types = typeMap
                signatures = functionSignatureMap
            }

let getLineForType customTypes f : int option = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None