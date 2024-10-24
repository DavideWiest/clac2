module rec Clac2.FrontEnd.Util

open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Lang.Primitive
open Clac2.Core.Lang.Language

let stringToType definitionContext s =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok |> Simple.toResult
    | _ -> Simple.toExcResult ("Unknown type: " + s)

let buildLoc fileLoc lineLoc = { fileLocation = fileLoc; lineLocation = lineLoc }

let nameIsInvalid (name: string) =
        name |> Seq.exists (fun x -> Seq.contains x Syntax.specialChars)
        || Primitive.isPrim name
        || Array.contains name FuncData.fnOptions
        || Array.contains name Types.baseTypes
        || Array.contains name Syntax.keywords