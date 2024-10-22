module rec Clac2.BuiltIn.Arithmetic

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Lang.Primitive
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.Util
open Clac2.BuiltIn.BuiltInFn

let toIntArray defVals = 
    defVals 
    |> Array.map DefinedValue.toPrimitive 
    |> Result.combineToArray 
    |> bind (Array.map Primitive.toInt >> Result.combineToArray)

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
            signature = [| Types.TInt; Types.TInt; Types.TInt |]
            args = [| "n1"; "n2" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = toIntArray
            innerFn = fun input -> Array.reduce f input |> Ok
            reconversionFn = PInt >> DefinedPrimitive
        }
    )
