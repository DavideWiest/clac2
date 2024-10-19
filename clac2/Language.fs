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
        || Array.contains name FunctionData.fnOptions
        || Array.contains name Types.baseTypes

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

module FunctionData =
    let fnOptions = [| "infix"; "postfix"; "noMemo"|]

module BuiltIn =
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
        |> Array.map (fun (k, fix, f) -> k, fix, fun input -> Conversion.fnTypeToIntAdapter k f input 2)
        |> Array.map (Conversion.fSharpFunctionToFn basicArithmeticArgsAndSignature Types.intType basicArithmeticOptions)

module StandardContext =
    let buildStandardContext (baseFuncs: DefinedCallableFunction array) (supportedTypes: string array) =
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
    let fSharpFunctionToFn (typedArgs: (string * FnType) array) (returnType: FnType) (baseFnOption: FnOptions) (extraData: string * OperatorFixation * DefinedFn) =
        let name, fixation, f = extraData
        {
            name = name
            signature = typedArgs |> Array.map snd |> Array.append [| returnType |]
            args = typedArgs |> Array.map fst
            DefinedFn = f
            fnOptions = { baseFnOption with fixation = fixation }
        }

    let rec fnTypeToIntAdapter name (f: int array -> int) (input: DefinedValue array) nArgs : Result<DefinedValue, string> =
        if input.Length > nArgs then Error ("Internal Error: Wrong number of arguments for fnTypeToIntAdapter for builtin function " + name) else

        if input.Length < nArgs then 
            Ok (DefinedFn (name + sprintf " with %i/%i args curried" input.Length nArgs, fun args -> fnTypeToIntAdapter name f (Array.append input args) nArgs))
        else

        let maybeDefinedInput = input |> Array.map (definedValueToInt) |> combineResultsToArray

        maybeDefinedInput
        |> bind (f >> PrimitiveInt >> DefinedPrimitive >> Ok)
