module rec Clac2.BuiltIn.Numeric

open Clac2.Core.Domain
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.Util
open Clac2.BuiltIn.BuiltInFn

let intFuncsOf2 = 
    [|

        ("+", Infix, (+))
        ("-", Infix, (-))
        ("*", Infix, (*))
        ("/", Infix, (/))
        ("^", Infix, pown)

        ("%", Infix, (%))
        
        ("max", Prefix, fun a b -> if a > b then a else b)
        ("min", Prefix, fun a b -> if a < b then a else b)

    |]
    |> Array.map (fun (name, fix, f) -> 
        {
            name = name
            signature = [| Types.TInt; Types.TInt; Types.TInt |]
            args = [| "n1"; "n2" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = DefinedValue.toIntArray
            innerFn = fun input -> Array.reduce f input |> Ok
            reconversionFn = PInt >> DefinedPrimitive
        }
    )
    |> Array.map BuiltInFn.initThroughAdapter

let intFuncsOf1 =
    [|
        ("abs", Prefix, abs)
        ("negate", Prefix, (~-))
    |]
    |> Array.map (fun (name, fix, f) -> 
        {
            name = name
            signature = [| Types.TInt; Types.TInt |]
            args = [| "n" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = DefinedValue.toIntArray
            innerFn = fun input -> f input.[0] |> Ok
            reconversionFn = PInt >> DefinedPrimitive
        }
    )
    |> Array.map BuiltInFn.initThroughAdapter

let floatFuncsOf2 = 
    [|
        ("+f", Infix, (+))
        ("-f", Infix, (-))
        ("*f", Infix, (*))
        ("/f", Infix, (/))
        ("^f", Infix, (fun a b -> a ** b))

        ("\%f", Infix, (%))

        ("maxf", Prefix, fun a b -> if a > b then a else b)
        ("minf", Prefix, fun a b -> if a < b then a else b)

    |]
    |> Array.map (fun (name, fix, f: float -> float -> float) -> 
        {
            name = name
            signature = [| Types.TFloat; Types.TFloat; Types.TFloat |]
            args = [| "f1"; "f2" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = DefinedValue.toFloatArray
            innerFn = fun input -> Array.reduce f input |> Ok
            reconversionFn = PFloat >> DefinedPrimitive
        }
    )
    |> Array.map BuiltInFn.initThroughAdapter

let floatFuncsOf1 =
    [|
        ("absf", Prefix, abs)
        ("negatef", Prefix, (~-))
    |]
    |> Array.map (fun (name, fix, f) -> 
        {
            name = name
            signature = [| Types.TFloat; Types.TFloat |]
            args = [| "f" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = DefinedValue.toFloatArray
            innerFn = fun input -> f input.[0] |> Ok
            reconversionFn = PFloat >> DefinedPrimitive
        }
    )
    |> Array.map BuiltInFn.initThroughAdapter


