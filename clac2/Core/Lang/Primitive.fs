module rec Clac2.Core.Lang.Primitive

open System
open Clac2.Core.Domain


module Primitive =
    let read (p: string) =
        
        if Seq.forall Char.IsDigit p then p |> int |> PInt |> Some else

        None

    let isPrim (s: string) = read s |> Option.isSome

    let toPrimOption p = 
        match p with
        | PInt _ -> PoInt

    let primOptionToFnType po =
        match po with
        | PoInt -> BaseFnType "int"

    let typeOfPrimitive p = p |> toPrimOption |> primOptionToFnType