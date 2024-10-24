module rec Clac2.Core.ProgramUtils

open System
open Clac2.Core.Domain
open Clac2.Core.Representation
open Clac2.Core.Lang.Primitive
open Clac2.Core.Lang.Language

let resultToReturnCode x =
    match x with 
    | Ok _ -> 0
    | Error _ -> 1

let consoleLoop executionFn origScopeCtx origCallableCtx s : int =
    let rec loop i sAcc s =
        if s = "exit" || s = "quit"  then 0 else

        match executionFn origScopeCtx origCallableCtx (Interactive (sAcc + "\n" + s)) with
        | Ok (newBindings: DefinedValue array) ->
            let bindingsStringArr: string array = buildBindingsString i newBindings[i..]
            let newSAcc = sprintf "%s\n%s\n%s" sAcc s (bindingsStringArr |> String.concat "\n")
            
            if bindingsStringArr.Length > 0 then printfn ""
            for binding in bindingsStringArr do
                printf "    %s\n" binding
            if bindingsStringArr.Length > 0 then printfn ""

            printf "> "
            let newS = Console.ReadLine()
            loop (newBindings.Length) newSAcc newS

        | Error () -> 
            printf "\n\n> "
            let newS = Console.ReadLine()
            loop i sAcc newS

    loop 0 "" s

let buildBindingsString iStart bindings = 
    bindings
    |> Array.mapi (fun i x -> sprintf "%s %s%i : %s = %s" Syntax.assignKeyword Syntax.consoleVariableIdent (iStart+i) (definedValueToType x) (ToString.definedValue x))

let definedValueToType dv =
    match dv with
    | DefinedFn _ -> failwith "Console mode does not support functions yet"
    | DefinedPrimitive p -> Primitive.typeOfPrimitive p