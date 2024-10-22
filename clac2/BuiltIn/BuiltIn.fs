module rec Clac2.BuiltIn.BuiltIn

open Clac2.BuiltIn.Numeric
open Clac2.BuiltIn.Boolean

let allFunctions =
    Array.concat [
        intFuncsOf2
        intFuncsOf1
        floatFuncsOf2
        floatFuncsOf1
        boolFuncsOf2
        equalityFuncs
        inequalityFuncs
    ]