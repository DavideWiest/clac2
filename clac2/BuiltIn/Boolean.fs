module rec Clac2.BuiltIn.Boolean

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Lang.Primitive
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.Util
open Clac2.BuiltIn.BuiltInFn

let toBoolArray defVals = 
    defVals 
    |> Array.map DefinedValue.toPrimitive 
    |> Result.combineToArray 
    |> bind (Array.map Primitive.toBool >> Result.combineToArray)

let boolFuncsOf2 = 
    [|
        ("and", Infix, (&&))
        ("or", Infix, (||))
        ("xor", Infix, (fun a b -> (a || b) && not (a && b)))
        ("nand", Infix, (fun a b -> not (a && b)))
    |]
    |> Array.map (fun (name, fix, f) -> 
        {
            name = name
            signature = [| Types.TBool; Types.TBool; Types.TBool |]
            args = [| "b1"; "b2" |]
            fnOptions = { genericFnOptions with fixation = fix }
            conversionFn = toBoolArray
            innerFn = fun input -> Array.reduce f input |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
    )
    |> Array.map BuiltInFn.initThroughAdapter

let equalityFuncs = 
    [|
        BuiltInFn.initThroughAdapter {
            name = "="
            signature = [| Types.TBool; Types.TBool; Types.TBool |]
            args = [| "b1"; "b2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = toBoolArray
            innerFn = fun input -> input[0] = input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
        BuiltInFn.initThroughAdapter {
            name = "=i"
            signature = [| Types.TInt; Types.TInt; Types.TBool |]
            args = [| "n1"; "n2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = DefinedValue.toIntArray
            innerFn = fun input -> input[0] = input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
        BuiltInFn.initThroughAdapter {
            name = "=f"
            signature = [| Types.TFloat; Types.TFloat; Types.TBool |]
            args = [| "f1"; "f2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = DefinedValue.toFloatArray
            innerFn = fun input -> input[0] = input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
    |]

let inequalityFuncs= 
    [|
        BuiltInFn.initThroughAdapter {
            name = "!="
            signature = [| Types.TBool; Types.TBool; Types.TBool |]
            args = [| "b1"; "b2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = toBoolArray
            innerFn = fun input -> input[0] <> input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
        BuiltInFn.initThroughAdapter {
            name = "!=i"
            signature = [| Types.TInt; Types.TInt; Types.TBool |]
            args = [| "n1"; "n2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = DefinedValue.toIntArray
            innerFn = fun input -> input[0] <> input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
        BuiltInFn.initThroughAdapter{
            name = "!=f"
            signature = [| Types.TFloat; Types.TFloat; Types.TBool |]
            args = [| "f1"; "f2" |]
            fnOptions = { genericFnOptions with fixation = Infix }
            conversionFn = DefinedValue.toFloatArray
            innerFn = fun input -> input[0] <> input[1] |> Ok
            reconversionFn = PBool >> DefinedPrimitive
        }
    |]
