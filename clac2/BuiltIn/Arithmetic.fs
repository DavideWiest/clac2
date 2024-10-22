module rec Clac2.BuiltIn.Arithmetic

open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.Interface

let toInt defVal =
    match defVal with
    | DefinedPrimitive (PInt i) -> Ok i
    | _ -> Error (sprintf "Expected int, got %A" defVal)

let toIntArray defVals = defVals |> Array.map toInt |> Result.combineToArray
let toDefinedValue n = n |> PInt |> DefinedPrimitive

let arithmeticFuncs = 
    [|
        ("add", Prefix, (+))
        ("subtract", Prefix, (-))
        ("mul", Prefix, (*))
        ("div", Prefix, (/))
        ("pow", Prefix, pown)
        ("+", Infix, (+))
        ("-", Infix, (-))
        ("*", Infix, (*))
        ("/", Infix, (/))
        ("^", Infix, pown)
    |]
    |> Array.map (fun (name, fix, f) -> 
        {
            name = name
            signature = [| Types.intType; Types.intType; Types.intType |]
            args = [| "n1"; "n2" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = toIntArray
            innerFn = fun input -> Array.reduce f input |> Ok
            reconversionFn = toDefinedValue
        }
    )

