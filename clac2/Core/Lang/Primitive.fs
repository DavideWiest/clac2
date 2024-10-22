module rec Clac2.Core.Lang.Primitive

open System
open Clac2.Core.Domain


module Primitive =
    let read (p: string) =
        
        if Seq.forall Char.IsDigit p then p |> int |> PInt |> Some else

        None

    let isPrim (s: string) = read s |> Option.isSome

    let primToFnType p = 
        match p with
        | PInt _ -> BaseFnType "int"
        | PFloat _ -> BaseFnType "float"

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