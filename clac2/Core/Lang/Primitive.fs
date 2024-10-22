module rec Clac2.Core.Lang.Primitive

open System
open Clac2.Core.Domain
open Clac2.Core.Lang.Language

module Primitive =
    let read (s: string) =
        // ints
        if StringIsInt s then s |> int |> PInt |> Some else
        // floats
        if s.Contains(".") then 
            if s.Substring(s.IndexOf(".") + 1) |> StringIsInt && s.Substring(0, s.IndexOf(".")) |> StringIsInt then
                s |> float |> PFloat |> Some
            else None
        else

        // bools
        if s = Constants.trueStr then true |> PBool |> Some else
        if s = Constants.falseStr then false |> PBool |> Some else

        None

    let StringIsInt (s: string) = 
        Seq.forall Char.IsDigit s || s.StartsWith("-") && Seq.forall Char.IsDigit (s.Substring(1))

    let isPrim (s: string) = read s |> Option.isSome

    let primToFnType p = 
        match p with
        | PInt _ -> "int"
        | PFloat _ -> "float"
        | PBool _ -> "bool"

    let typeOfPrimitive p = p |> primToFnType

    let toInt p =
        match p with
        | PInt i -> Ok i
        | _ -> Error (converisonErrMsg "int" p)

    let toFloat p =
        match p with
        | PFloat f -> Ok f
        | _ -> Error (converisonErrMsg "float" p)

    let toBool p =
        match p with
        | PBool b -> Ok b
        | _ -> Error (converisonErrMsg "bool" p)

    let converisonErrMsg expected got = sprintf "Conversion error: expected %s, got %A" expected got
