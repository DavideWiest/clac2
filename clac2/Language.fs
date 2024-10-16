module rec Clac2.Language

open Clac2.Domain
open Clac2.Utilities
open Clac2.DomainUtilities
open FSharp.Core.Result

module Syntax =
    let commentIdentifer = "--"
    let nameUnallowedChars = " \t\n\r;:()[]{}" // this language is flexible - hence negative validation

    let nameIsInvalid (name: string) =
        name 
        |> Seq.exists (fun x -> Seq.contains x nameUnallowedChars)
        || isPrimitive name

module Types =
    let baseTypes = [| "int" |]

    let intType = BaseFnType "int"

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

// module Modules = 
    // let reservedModuleName = [| "Clac", "Std" |]

module BuildIn =
    let basicArithmeticArgsAndSignature = [| ("n1", Types.intType); ("n2", Types.intType) |]

    let arithmeticFuncs = 
        [|
            ("add", (+))
            ("subtract", (-))
            ("mul", (*))
            ("div", (/))
            ("pow", (fun x y -> int (float x ** float y)))
        |]
        |> Array.map (fun (k, f) -> k, fun input -> Array.reduce f input)
        |> Array.map (fun (k, f) -> k, fun input -> Conversion.fnTypeToIntAdapter k f input 2)
        |> Array.map (Conversion.fSharpFunctionToFn basicArithmeticArgsAndSignature Types.intType)

module StandardContext =
    let buildStandardContext (baseFuncs: BuiltInFn array) (supportedTypes: string array) =
        {
            defCtx = {
                types = supportedTypes
                functions = baseFuncs |> Array.map (fun x -> x.name)
            }
            definedCtx = {
                functions = baseFuncs
            }
        }

module Conversion = 
    let fSharpFunctionToFn (typedArgs: (string * FnType) array) (returnType: FnType) (nameAndFn: string * DefinedManipulation) =
        {
            name = nameAndFn |> fst
            signature = typedArgs |> Array.map snd |> Array.append [| returnType |]
            args = typedArgs |> Array.map fst
            definedManip = nameAndFn |> snd
        }

    let rec fnTypeToIntAdapter name (f: int array -> int) (input: DefinedValue array) nArgs : Result<DefinedValue, string> =
        if input.Length > nArgs then Error ("Internal Error: Wrong number of arguments for fnTypeToIntAdapter for builtin function " + name) else

        if input.Length < nArgs then 
            Ok (DefinedFn (name + sprintf " with %i/%i args curried" input.Length nArgs, fun args -> fnTypeToIntAdapter name f (Array.append input args) nArgs))
        else

        let maybeDefinedInput = input |> Array.map (definedValueToInt) |> combineResultsToArray

        maybeDefinedInput
        |> bind (f >> PrimitiveInt >> DefinedPrimitive >> Ok)
