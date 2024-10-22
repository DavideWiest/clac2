module rec Clac2.Core.Lang.Language

open Clac2.Core.Domain
open Clac2.Core.Lang.Primitive

module Syntax =
    let commentIdentifer = "--"
    // names cannot contain these characters
    // this language is flexible - hence negative validation
    let specialChars = " \t\n\r;:()[]{}"
    let reservedVariables = [| "#" |]

    let nameIsInvalid (name: string) =
        name |> Seq.exists (fun x -> Seq.contains x specialChars)
        || Primitive.isPrim name
        || Array.contains name FuncData.fnOptions
        || Array.contains name Types.baseTypes
        || Array.contains name reservedVariables

module FuncData =
    let fnOptions = [| "infix"; "postfix"; "noMemo" |]

module Types =
    let baseTypes = [| "int"; "float" |]
    
    let TInt = BaseFnType "int"
    let TFloat = BaseFnType "float"
    let TBool = BaseFnType "bool"

module Files =
    let officialExtensions = [| "clac" |]
    let standardFileDir = "Std"
    let standardFileLocations = 
        [| "Standard.clac"; "Numeric.clac" |] 
        |> Array.map (fun p -> System.IO.Path.Combine (standardFileDir, p)) 
        |> Array.map System.IO.Path.GetFullPath

    let packageLocation = "clacPackages"
    let toQualifiedFileLoc dir (fileRef: string) = 
        fileRef.Split [| '/' |]
        |> System.IO.Path.Combine
        |> fun reference -> System.IO.Path.Combine(dir, reference + if Array.exists (fun (ending: string) -> reference.EndsWith(ending)) officialExtensions then "" else "." + officialExtensions[0]) 
        |> System.IO.Path.GetFullPath

