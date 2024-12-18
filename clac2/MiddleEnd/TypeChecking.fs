module rec Clac2.MiddleEnd.TypeChecker

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Exc.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Lang.Primitive
open Clac2.MiddleEnd.TypeCheckingCtx
open Clac2.MiddleEnd.MiddleEndUtils

let validateProgramTypes scopeCtx callableCtx (programAndDepMap: Program * fileDependencyMap) : FullResult<Program> =
    let (program, _) = programAndDepMap
    let stdCtx = StdCtx.init callableCtx scopeCtx

    let validateFileArray (fileArr: File array) =
        fileArr
        |> Array.map (fun file -> file.location, file.content)
        |> Array.map (fun (loc, orderedFile) -> 
            Some loc, orderedFile |> validateTypes stdCtx program false
        )
        |> Array.map Full.tupledToExc
        |> Result.combineToArray

    program.mainFile.content
    |> (validateTypes stdCtx program true)
    |> Full.toResult program.mainFile.maybeLocation
    |> bind (fun _ -> validateFileArray program.secondaryFiles)
    |> map (fun _ -> program)
    
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


let checkTypeDefinitions (stdCtx: StandardContext) typeCheckingCtx customTypes =    
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
            if Array.contains x stdCtx.scope.types then None else

            let line = getLineForType typeCheckingCtx.types x
            if List.contains x typesHigherUp' then Some(Intermediate.ExcFPPureMaybeLine ("Recursive type definition: " + x) line) else 
            if Map.containsKey x typeCheckingCtx.types |> not then Some(Intermediate.ExcFPPureMaybeLine ("Unknown type: " + x) line) else

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

type ExcpectedOutputType = AnyOut | ExpectedType of FnType

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
        if expectedOutputType = AnyOut then None else Some (Intermediate.ExcFPPure ("Got empty manipulation when expecting output type of: " + expectedOutputType.ToString()) line)
    else

    match m[0] with
    | Fn f ->
        let maybePrim = Primitive.read f
        if maybePrim.IsSome then
            if m.Length > 1 then Some (Intermediate.ExcFPPure ("Primitive " + f + " used as function.") line) else

            let primitiveType = Primitive.typeOfPrimitive maybePrim.Value
            if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType (BaseFnType primitiveType) then Some (Intermediate.ExcFPPure (sprintf "Expected output type for %s is %A, received %A" f expectedOutputType primitiveType) line) else None
        else

        if not (typeCheckingCtx.signatures.ContainsKey f) then Some (Intermediate.ExcWithoutLine (Simple.Exc ("Internal Error: customFnsMap does not contain function " + f + ". It probably was not registered in the front end."))) else

        let signature = typeCheckingCtx.signatures[f]
        if m.Length - 1 > signature.Length - 1 then Some (Intermediate.ExcFPPure (sprintf "Too many arguments for %s: Expected %i, got %i." f (signature.Length-1) (m.Length-1)) line) else

        let inputIStop = m.Length - 2
        let inputSignature, outputSignature = signature[..inputIStop], signature[inputIStop+1..]
        let preparedOutputSignature = if outputSignature.Length = 1 then outputSignature[0] else Function outputSignature

        if expectedOutputType <> AnyOut && expectedOutputType <> ExpectedType preparedOutputSignature then Some (Intermediate.ExcFPPure (sprintf "Expected type for symbol (here %s) is %A, received %A" f expectedOutputType outputSignature) line) else
        
        typesMatch inputSignature m[1..]
    | Manipulation m' -> 
        // the first manipulation is a curried function, so we can append the first args to the rest
        let newManip = Array.concat [ m' ; m.[1..] ]
        checkManipulation typeCheckingCtx newManip line expectedOutputType
