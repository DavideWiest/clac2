module rec Clac2.BuiltIn.BuiltIn

open Clac2.BuiltIn.BuiltInFn
open Clac2.BuiltIn.Arithmetic

let allFunctions =
    Array.concat [
        arithmeticFuncs
    ]
    |> Array.map BuiltInFn.initThroughAdapter