module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities


module TypeChecking =
    let validateTypes (stdCtx: StandardContext) (lines: Line array) =
        match checkTypesInner stdCtx lines with
        | None -> Ok lines
        | Some e -> Error e

    let checkTypesInner (stdCtx: StandardContext) (lines: Line array) = 
        // check types, then manipulations, then assignemtns

        let customTypes = lines |> Array.choose (fun x -> match x with | TypeDefinition t -> Some t | _ -> None)
        let customAssignments = lines |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
        let customTypeMap = customTypes |> Array.map (fun x -> x.name, x) |> Map.ofArray
        let manipulations = lines |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)

        let functionSignatureMap = 
            Array.concat [
            stdCtx.definedCtx.functions |> Array.map (fun x -> x.name, x.signature); 
            customAssignments |> Array.map (fun x -> x.name, x.signature)
            ] 
            |> Map.ofArray

        checkTypeDefinitions stdCtx customTypes customTypeMap 
        |> Option.orElse (checkManipulations manipulations functionSignatureMap)
        |> Option.orElse (checkAssignments functionSignatureMap customAssignments) 

    let checkTypeDefinitions (stdCtx: StandardContext) customTypes customTypeMap : string option =    
        // no recursive type definitions - type system is a tree
        let rec checkType (customTypeMap: Map<string,TypeDefinition>) (typesHigherUp: string list) (x: TypeDefinition) =
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

                if List.contains x typesHigherUp' then Some ("Recursive type definition: " + x) else 
                if Map.containsKey x customTypeMap |> not then Some ("Type not defined: " + x) else

                checkType customTypeMap typesHigherUp' customTypeMap[x]
            )
        
        Array.tryPick (checkType customTypeMap []) customTypes

    let checkManipulations manipulations functionSignatureMap : string option =
        let typesMatch inputSignature args f =
            args
            |> Array.zip inputSignature
            |> Array.tryPick (fun (x, y) -> if x = y then None else Some (sprintf "Argument type mismatch: Expected %A, but got %A for function %s." x y f))

        let rec checkManipulation (m: Reference array) = 
            match m[0] with
            | Fn f ->
                if isPrimitive f then None else

                let signature = functionSignatureMap[f]
                let inputSignature= signature[..signature.Length-2]
                if inputSignature.Length > m.Length - 1 then Some ("Too few arguments for function: " + f) else

                let args = m[1..inputSignature.Length]
                let typesOfArgs = args |> Array.map (ReferenceToFnType functionSignatureMap)

                if m.Length - 1 = inputSignature.Length then typesMatch inputSignature typesOfArgs f else

                // prevent Array.last from throwing exception
                if inputSignature.Length = 0 then Some ("Too many arguments for function " + f) else

                // check the other types
                let typesUntilLastMatch = typesMatch inputSignature[..inputSignature.Length-2] typesOfArgs[..typesOfArgs.Length-2] f
                if typesUntilLastMatch |> Option.isSome then typesUntilLastMatch else

                // if too many arguments exist, try to pass them into the last argument
                let lastArg = args |> Array.last

                match lastArg with
                // extra case needed for clarity
                | Fn f' -> 
                    let lastInputType = inputSignature |> Array.last
                    let outputSignature' = functionSignatureMap[f'] |> Array.last

                    if lastInputType <> outputSignature' then Some (sprintf "Argument type mismatch: Expected %A, but got %A. Trying to push superfluous arguments of function %s to last element." lastInputType outputSignature' f) else

                    checkManipulation m[inputSignature.Length..]

        Array.tryPick checkManipulation manipulations

    let checkAssignments functionSignatureMap customAssignments : string option =
        let checkAssignment (assignment: CallableFunction) =
            let argumentsAsAssignments = 
                assignment.args 
                |> Array.mapi (fun i x -> 
                    (x, [| assignment.signature[i] |])
                )
                |> Map.ofArray

            functionSignatureMap
            |> Map.merge argumentsAsAssignments
            |> checkManipulations [| assignment.fn |]

        Array.tryPick checkAssignment customAssignments

let ReferenceToFnType (functionSignatureMap: Map<string,FnType array>) (x: Reference) =
    match x with
    | Fn f -> 
        if isPrimitive f then getPrimitiveType f else

        let s = functionSignatureMap[f]
        // variables are treated as functions within FnType, but can not be within the Reference type
        if s.Length = 1 then s[0] else Function s
