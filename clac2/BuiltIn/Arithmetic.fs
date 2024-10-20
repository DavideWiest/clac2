module rec Clac2.BuiltIn.Arithmetic

open Clac2.Core.Domain
open Clac2.Core.Lang.Language
open Clac2.BuiltIn.Util

let basicArithmeticArgsAndSignature = [| ("n1", Types.intType); ("n2", Types.intType) |]
let basicArithmeticOptions = { fixation = Prefix; noMemo = false }

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
    |> Array.map (fun (k, fix, f) -> k, fix, fun input -> Array.reduce f input)
    |> Array.map (fun (k, fix, f) -> k, fix, fun input -> Conversion.fnTypeToTAdapter PoInt k f input 2)
    |> Array.map (Conversion.fSharpFunctionToFn basicArithmeticArgsAndSignature Types.intType basicArithmeticOptions)