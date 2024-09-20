module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities

module TypeChecking =
    let validateTypes (stdCtx: StandardContext) (file: OrderedFile): IntermediateClacResult<OrderedFile> =
        match checkTypesInner stdCtx file with
        | None -> Ok file
        | Some e -> Error e

    let checkTypesInner (stdCtx: StandardContext) (file: OrderedFile) : IntermediateException option = 
        // check types, then manipulations, then assignemtns

        let customTypeMap = file.typeDefinitions |> Array.map (fun x -> x.name, x) |> Map.ofArray

        let functionSignatureMap = 
            [
                stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature)
                file.assignments |> Array.map (fun x -> x.name, x.signature)
            ] 
            |> Array.concat
            |> Map.ofArray

        let customFnsMap = 
            file.assignments 
            |> Array.map (fun x -> x.name, x) 
            |> Map.ofArray

        checkTypeDefinitions stdCtx file.typeDefinitions customTypeMap 
        |> Option.orElse (checkManipulations customFnsMap functionSignatureMap file.expressions)
        |> Option.orElse (checkAssignments customFnsMap functionSignatureMap file.assignments) 

    let checkTypeDefinitions (stdCtx: StandardContext) customTypes customTypeMap : IntermediateException option =    
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
                if Map.containsKey x customTypeMap |> not then Some(IntermediateExcFPPure ("Type not defined: " + x) line) else

                checkTypeDefsInner customTypeMap typesHigherUp' customTypeMap[x]
            )
        
        Array.tryPick (checkTypeDefsInner customTypeMap []) customTypes

    let checkManipulations customFnsMap functionSignatureMap manipulations : IntermediateException option =
        manipulations
        |> Array.map (fun x -> x.manipulation)
        |> Array.tryPick (checkManipulation customFnsMap functionSignatureMap)

    let rec checkManipulation customFnsMap functionSignatureMap (m: Reference array) = 
        let typesMatch inputSignature args f =
            args
            |> Array.zip inputSignature
            |> Array.tryPick (fun (x, y) -> 
                if x = y then None else Some (GenExc (sprintf "Argument type mismatch: Expected %A, but got %A for function %s." x y f) )
            )
            
        match m[0] with
        | Fn f ->
            if isPrimitive f then None else

            let line = getLine customFnsMap f

            let signature = functionSignatureMap[f]
            let inputSignature= signature[..signature.Length-2]
            if inputSignature.Length > m.Length - 1 then Some (IntermediateExcFPPure ("Too few arguments for function: " + f) line) else

            let args = m[1..inputSignature.Length]
            let typesOfArgs = args |> Array.map (ReferenceToFnType functionSignatureMap)

            if m.Length - 1 = inputSignature.Length then typesMatch inputSignature typesOfArgs f |> Option.map (IntermediateExc line) else

            // prevent Array.last from throwing exception
            if inputSignature.Length = 0 then Some (IntermediateExcFPPure ("Too many arguments for function " + f) line) else

            // check the other types
            let typesUntilLastMatch = typesMatch inputSignature[..inputSignature.Length-2] typesOfArgs[..typesOfArgs.Length-2] f
            if typesUntilLastMatch |> Option.isSome then typesUntilLastMatch |> Option.map (IntermediateExc line) else

            // if too many arguments exist, try to pass them into the last argument
            let lastArg = args |> Array.last

            match lastArg with
            // extra case needed for clarity
            | Fn f' -> 
                let lastInputType = inputSignature |> Array.last
                let outputSignature' = functionSignatureMap[f'] |> Array.last

                if lastInputType <> outputSignature' then Some(IntermediateExcFPPure (sprintf "Argument type mismatch: Expected %A, but got %A. Trying to push superfluous arguments of function %s to last element." lastInputType outputSignature' f) line) else

                checkManipulation customFnsMap functionSignatureMap m[inputSignature.Length..]

    let checkAssignments customFnsMap functionSignatureMap customAssignments : IntermediateException option =
        let checkAssignment (assignment: CallableFunction) =
            let argumentsAsAssignments = 
                assignment.args 
                |> Array.mapi (fun i x -> 
                    (x, [| assignment.signature[i] |])
                )
                |> Map.ofArray

            let newfnSignatureMap = Map.merge argumentsAsAssignments functionSignatureMap
            checkManipulation customFnsMap newfnSignatureMap assignment.fn

        Array.tryPick checkAssignment customAssignments

let ReferenceToFnType (functionSignatureMap: Map<string,FnType array>) (x: Reference) =
    match x with
    | Fn f -> 
        if isPrimitive f then getPrimitiveType f else

        let s = functionSignatureMap[f]
        // variables are treated as functions within FnType, but can not be within the Reference type
        if s.Length = 1 then s[0] else Function s

let getLine customFnsMap f = customFnsMap[f].loc.lineLocation
let getLineType customTypes f = customTypes[f].loc.lineLocation