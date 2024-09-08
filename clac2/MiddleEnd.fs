module rec Clac2.MiddleEnd

open Clac2.Domain
open Clac2.Utilities


// check types

let checkTypes (stdCtx: StandardContext) (lines: Line array) =
    match checkTypesInner stdCtx lines with
    | None -> Ok lines
    | Some e -> Error e

let checkTypesInner (stdCtx: StandardContext) (lines: Line array) = 
    // check types, then manipulations, then assignemtns

    let customTypes = lines |> Array.choose (fun x -> match x with | TypeDefinition t -> Some t | _ -> None)
    let customAssignments = lines |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
    let customTypeMap = customTypes |> Array.map (fun x -> x.name, x) |> Map.ofArray

    let manipulations = 
        lines 
        |> Array.choose (fun x -> 
            match x with 
            | Expression e -> Some e
            | Assignment a -> Some a.fn
            | _ -> None
        )

    checkTypeDefinitions stdCtx customTypes customTypeMap 
    |> Option.orElse (checkManipulations stdCtx manipulations customAssignments customTypeMap)
    |> Option.orElse (checkAssignments stdCtx customAssignments) 

let checkTypeDefinitions (stdCtx: StandardContext) customTypes customTypeMap : string option =

    let allTypeNames = Array.concat [getKeys customTypeMap; stdCtx.defCtx.types]
    let maybeDuplicates = hasDuplicatesByReturning allTypeNames id
    if maybeDuplicates.Length > 0 then Some ("Duplicate type definitions: " + (maybeDuplicates |> String.concat ", ")) else
    
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
            if List.contains x typesHigherUp' then Some ("Recursive type definition: " + x) else checkType customTypeMap typesHigherUp' customTypeMap[x]
        )
    
    Array.tryPick (checkType customTypeMap []) customTypes

let checkManipulations (stdCtx: StandardContext) manipulations customAssignments customTypeMap : string option =


let checkAssignments (stdCtx: StandardContext) customAssignments : string option =
    None

let manipulationFunctionHierarchyMapping manipulation types =
    