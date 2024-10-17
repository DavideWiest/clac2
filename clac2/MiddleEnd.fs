module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities

module Normalization =
    let normalizePrograms stdCtx program depMap =
        let reconstructedProgram = applyNormalizationPipeline stdCtx program
        (reconstructedProgram, depMap)

    let applyNormalizationPipeline stdCtx program =
        normalizationPipeline
        |> Array.fold (fun acc f -> f stdCtx acc) program

    let normalizationPipeline: (StandardContext -> Program -> Program) array = [|
        nestArgumentPropagations
        operatorFixationCorrection
    |]

    let nestArgumentPropagations stdCtx program =
        let functionSignatureMap = TypeChecking.TypeCheckingCtx.generateFunctionSignatureMap stdCtx program (Some program.mainFile.content)

        // argument propagation is not supported for curried functions either
        let rec nestArgumentPropagationsInner (functionSignatureMap: Map<string,Signature>) manipParent =
            let propagate (m: Manipulation) sigLen = if m.Length - 1 <= sigLen - 1 then m else Array.append m[..sigLen-2] [| Manipulation m[sigLen-1..] |]
            match manipParent with
            | Manip m -> 
                match m[0] with
                | Fn f -> 
                    // prefix operators
                    if isPrimitive f |> not then propagate m functionSignatureMap[f].Length
                    // infix operators
                    elif m.Length = 1 then m else
                    match m[1] with
                    | Fn f' -> if isPrimitive f' then m else propagate m functionSignatureMap[f'].Length
                    | _ -> m
                    // propagation not supported for postfix operators
                | Manipulation m' -> nestArgumentPropagationsInner functionSignatureMap (Manip (Array.append m'  m.[1..]))
            | CallableFunction a -> nestArgumentPropagationsInner (TypeChecking.Signature.addArgsToSignatureMap a functionSignatureMap) (Manip a.manip)

        mapAllManipulations program (nestArgumentPropagationsInner functionSignatureMap)
        
    let operatorFixationCorrection stdCtx program =
        let fixationMap =
            (Array.append (program.secondaryFiles |> Array.map (fun x -> x.content)) [| program.mainFile.content |])
            |> Array.map extractFixationFromFile
            |> Array.concat
            |> Map.ofArray

        mapAllManipulations program (operatorFixationToPrefixOuter fixationMap)

    let extractFixationFromFile orderedFile =
        orderedFile.assignments
        |> Array.map (fun x -> x.name, x.fnOptions.fixation)

    type ManipulationWrapper = Manip of Manipulation | CallableFunction of CallableFunction

    let operatorFixationToPrefixOuter fixationMap manip =
        match manip with
        | Manip m -> operatorFixationToPrefix fixationMap m
        | CallableFunction fn -> operatorFixationToPrefix fixationMap fn.manip
        
    let operatorFixationToPrefix fixationMap manipFirst =
        let fixationFixOneManip (manip: Manipulation) =
            match manip with
            | [| _ |] -> manip
            | _ when fixationMapLookup fixationMap manip.[manip.Length - 1] = Some Postfix -> Array.append [| manip.[manip.Length - 1] |] manip.[..manip.Length - 2]
            | [| f; s; t |] when fixationMapLookup fixationMap s = Some Infix -> [| s; f; t |]
            | _ -> manip

        let rec operatorFixationToPrefixOneLevel (x: Reference) =
            match x with
            | Fn f -> Fn f
            | Manipulation manip -> fixationFixOneManip manip |> Array.map operatorFixationToPrefixOneLevel |> Manipulation
        
        manipFirst
        |> fixationFixOneManip

    let fixationMapLookup fixationMap x =
        match x with
        | Fn fn -> if fixationMap.ContainsKey fn then Some fixationMap[fn] else None // otherwise its a primitive
        | Manipulation m -> None
    
    let mapAllManipulations program mapF = 
        let mapExpression (e: FreeManipulation) =  { e with manip = mapF (Manip e.manip) }
        let mapAssignment (a: CallableFunction) = { a with manip = mapF (CallableFunction a) }
        let mapFile f = { f with expressions = f.expressions |> Array.map mapExpression; assignments = f.assignments |> Array.map mapAssignment }

        let newMainFile = { program.mainFile with content = mapFile program.mainFile.content }
        let newSecondaryFiles = program.secondaryFiles |> Array.map (fun f -> { f with content = mapFile f.content })

        { program with mainFile = newMainFile; secondaryFiles = newSecondaryFiles }


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
            if isPrimitive f then
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

        let generateFunctionSignatureMap stdCtx program file =
            let typesInFile = if file.IsNone then [||] else file.Value.assignments |> Array.map (fun x -> x.name, x.signature)
            [
                typesInFile
                program.secondaryFiles |> Array.map (fun f -> f.content.assignments |> Array.map (fun x -> x.name, x.signature)) |> Array.concat
                stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature)
            ]
            |> Array.concat
            |> Map.ofArray

let getLineForType customTypes f  = if customTypes.ContainsKey f then Some customTypes[f].loc.lineLocation else None