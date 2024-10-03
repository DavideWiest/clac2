module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities
open Clac2.Language

module TypeChecking =
    type TypeCheckingCtx = {
        types: Map<string, TypeDefinition>
        signatures: Map<string, Signature>
    }

    type ExpectedInputSignature =
        | AnyIn
        | ExpectedSignature of FnType array

    type ExcpectedOutputType =
        | AnyOut
        | ExpectedType of FnType

    let validateTypes (stdCtx: StandardContext) (program: Program) (file: OrderedFile) : IntermediateClacResult<OrderedFile> =
        match validateTypesInner stdCtx program file with
        | None -> Ok file
        | Some e -> Error e

    // check types, then manipulations, then assignments
    let validateTypesInner (stdCtx: StandardContext) (program: Program) (file: OrderedFile) : IntermediateException option = 

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
        
        let typeCheckingCtx = {
            // customFns = customFnsMap
            types = typeMap
            signatures = functionSignatureMap
        }

        checkTypeDefinitions stdCtx file.typeDefinitions typeMap 
        |> Option.orElse (checkManipulationsAnyOutputType typeCheckingCtx file.expressions)
        |> Option.orElse (checkAssignments typeCheckingCtx file.assignments) 

    // not necessary to check all types, only custom ones (will run for each file)
    let checkTypeDefinitions (stdCtx: StandardContext) customTypes typeMap : IntermediateException option =    
        // no recursive type definitions - type system is a tree
        let rec checkTypeDefsInner (customTypeMap: Map<string,TypeDefinition>) (typesHigherUp: string list) (x: TypeDefinition) =
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

                let line = getLineType customTypeMap x
                if List.contains x typesHigherUp' then Some(IntermediateExcFPPureMaybeLine ("Recursive type definition: " + x) line) else 
                if Map.containsKey x customTypeMap |> not then Some(IntermediateExcFPPureMaybeLine ("Unknown type: " + x) line) else

                checkTypeDefsInner customTypeMap typesHigherUp' customTypeMap[x]
            )
        
        Array.tryPick (checkTypeDefsInner typeMap []) customTypes

    let checkManipulationsAnyOutputType typeCheckingCtx manipulations : IntermediateException option =
        manipulations
        |> Array.map (fun x -> x.loc.lineLocation, x.manipulation)
        |> Array.tryPick (fun (i, x) -> checkManipulation typeCheckingCtx x i AnyIn AnyOut)

    let checkAssignments typeCheckingCtx customAssignments : IntermediateException option =
        let checkAssignment (assignment: CallableFunction) =
            let argumentsAsAssignments = 
                assignment.args 
                |> Array.mapi (fun i x -> 
                    (x, [| assignment.signature[i] |])
                )
                |> Map.ofArray

            let newTypeCheckingCtx = { typeCheckingCtx with signatures = Map.merge argumentsAsAssignments typeCheckingCtx.signatures }
            let expectedInputSignature = ExpectedSignature assignment.signature[..assignment.signature.Length-2]
            let expectedOutputType = ExpectedType assignment.signature[assignment.signature.Length-1]
            checkManipulation newTypeCheckingCtx assignment.fn assignment.loc.lineLocation expectedInputSignature expectedOutputType

        Array.tryPick checkAssignment customAssignments

    let rec checkManipulation typeCheckingCtx (m: Reference array) line (expectedInputSignature: ExpectedInputSignature) (expectedOutputType: ExcpectedOutputType) = 
        let rec typesMatch (inputSignature: FnType array) args f : IntermediateException option =
            Array.fold (fun acc (signaturePart: FnType, arg) -> 
                acc
                |> Option.orElse (
                    match arg with
                    | Fn f' ->
                        let signature = getInputSignature typeCheckingCtx f'
                        if signature.Length = 1 && signature[0] = signaturePart then None else Some (IntermediateExcFPPure (sprintf "Argument type mismatch: Expected %A, but got %A for function %s." signaturePart signature f) line) 
                    | Manipulation m' ->
                        match signaturePart with
                        | BaseFnType s ->
                            // constants/variables are treated as functions with no arguments
                            if m'.Length <> 1 then Some (IntermediateExcFPPure ("Expected constant (function without arguments), but received function.") line) else
                            if m'[0] <> Fn s then Some (IntermediateExcFPPure (sprintf "Expected constant of type %s, but received %A." s m'[0]) line) else None
                        | Function fs -> 
                            checkManipulation typeCheckingCtx m' line AnyIn (ExpectedType signaturePart) 
                            |> Option.orElse(typesMatch fs m' f)
                )
            ) None (Array.zip inputSignature args)

        match m[0] with
        | Fn f ->
            if isPrimitive f then
                if m.Length <> 1 then Some (IntermediateExcFPPure ("Primitive " + f + " used as function.") line) else None
            else

            if not (typeCheckingCtx.signatures.ContainsKey f) then Some (IntermediateExcWithoutLine (GenExc ("Internal Error: customFnsMap does not contain function " + f))) else

            let inputSignature, outputSignature = getSignatureInOutSeparate typeCheckingCtx f
            // implement argument propagation here and below the if statement
            if inputSignature.Length <> m.Length - 1 then Some (IntermediateExcFPPure (sprintf "Invalid number of arguments for %s: Expected %i, received %i" f inputSignature.Length (m.Length - 1)) line) else
            
            if expectedInputSignature <> AnyIn && expectedInputSignature <> ExpectedSignature inputSignature then Some (IntermediateExcFPPure (sprintf "Expected input signature for %s is %A, but received %A" f inputSignature expectedInputSignature) line) else
            if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType outputSignature then Some (IntermediateExcFPPure (sprintf "Expected output type for %s is %A, but received %A" f outputSignature expectedOutputType) line) else
        
            typesMatch inputSignature m[1..] f
        | Manipulation m' -> 
            // the first manipulation is a curried function, so we can append the first args to the rest
            let newManip = Array.concat [ m' ; m.[1..] ]
            checkManipulation typeCheckingCtx newManip line expectedInputSignature expectedOutputType

    let getInputSignature (typeCheckingCtx: TypeCheckingCtx) f : FnType array = 
        getSignatureInOutSeparate typeCheckingCtx f |> fst

    let getSignatureInOutSeparate (typeCheckingCtx: TypeCheckingCtx) f : FnType array * FnType = 
        let signature = getSignature typeCheckingCtx f
        signature[..signature.Length-2], signature[signature.Length-1]

    let getSignature (typeCheckingCtx: TypeCheckingCtx) f : Signature = 
        typeCheckingCtx.signatures[f]

let getLineType customTypes f : int option = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None