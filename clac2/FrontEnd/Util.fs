module rec Clac2.FrontEnd.Util

open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions

let stringToType definitionContext s =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok |> Simple.toResult
    | _ -> Simple.toExcResult ("Unknown type: " + s)