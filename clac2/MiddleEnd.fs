module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities


module TypeChecking =
    type TypeCheckingCtx = {
        types: Map<string, TypeDefinition>
        signatures: Map<string, Signature>
    }

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
        |> Option.orElse (checkManipulations typeCheckingCtx file.expressions)
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
                if List.contains x typesHigherUp' then Some(IntermediateExcFPPure ("Recursive type definition: " + x) line) else 
                if Map.containsKey x customTypeMap |> not then Some(IntermediateExcFPPure ("Unknown type: " + x) line) else

                checkTypeDefsInner customTypeMap typesHigherUp' customTypeMap[x]
            )
        
        Array.tryPick (checkTypeDefsInner typeMap []) customTypes

    let checkManipulations typeCheckingCtx manipulations : IntermediateException option =
        manipulations
        |> Array.map (fun x -> x.loc.lineLocation, x.manipulation)
        |> Array.tryPick (fun (i, x) -> checkManipulation typeCheckingCtx x i)

    let rec checkManipulation typeCheckingCtx (m: Reference array) line = 
        let typesMatch inputSignature args f =
            args
            |> Array.zip inputSignature
            |> Array.tryPick (fun (x, y) -> 
                if x = y then None else Some (GenExc (sprintf "Argument type mismatch: Expected %A, but got %A for function %s." x y f) )
            )
            
        match m[0] with
        | Fn f ->
            if isPrimitive f then None else

            if not (typeCheckingCtx.signatures.ContainsKey f) then Some (IntermediateExcWithoutLine (GenExc ("Internal Error: customFnsMap does not contain function " + f))) else

            let line = getAssignmentLine typeCheckingCtx.types f |> Option.orElse (getLineType typeCheckingCtx.types f)

            let signature = typeCheckingCtx.signatures[f]
            let inputSignature= signature[..signature.Length-2]
            if inputSignature.Length <> m.Length - 1 then Some (IntermediateExcFPPure ("Invalid number of arguments: " + f) line) else

            let args = m[1..inputSignature.Length]
            let typesOfArgs = args |> Array.map (ReferenceToFnType typeCheckingCtx.signatures)

            typesMatch inputSignature typesOfArgs f |> Option.map (IntermediateExcMaybeLine line)

    let checkAssignments typeCheckingCtx customAssignments : IntermediateException option =
        let checkAssignment (assignment: CallableFunction) =
            let argumentsAsAssignments = 
                assignment.args 
                |> Array.mapi (fun i x -> 
                    (x, [| assignment.signature[i] |])
                )
                |> Map.ofArray

            let newfnSignatureMap = Map.merge argumentsAsAssignments typeCheckingCtx.signatures
            let newTypeCheckingCtx = { typeCheckingCtx with signatures = newfnSignatureMap }
            checkManipulation newTypeCheckingCtx assignment.fn assignment.loc.lineLocation

        Array.tryPick checkAssignment customAssignments

let ReferenceToFnType (functionSignatureMap: Map<string,FnType array>) (x: Reference) =
    match x with
    | Fn f -> 
        if isPrimitive f then getPrimitiveType f else

        let s = functionSignatureMap[f]
        // variables are treated as functions within FnType, but can not be within the Reference type
        if s.Length = 1 then s[0] else Function s

let getAssignmentLine customFnsMap f : int option = if customFnsMap.ContainsKey f then Some customFnsMap[f].loc.lineLocation else None
let getLineType customTypes f : int option = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None