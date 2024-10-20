module rec Clac2.Core.Language

open System
open Clac2.Core.Utils
open Clac2.Core.Domain
open FSharp.Core.Result

module Syntax =
    let commentIdentifer = "--"
    // names cannot contain these characters
    // this language is flexible - hence negative validation
    let specialChars = " \t\n\r;:()[]{}"

    let nameIsInvalid (name: string) =
        name 
        |> Seq.exists (fun x -> Seq.contains x specialChars)
        || Primitive.isPrim name
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

module DefCtx =
    let getDefCtxWithStdCtxFromMap stdCtx (depMap: fileDependencyMap) fileLoc =
        mergeDefCtx stdCtx.defCtx depMap[fileLoc]
    
    let mergeDefCtxFromStdCtx stdCtx defCtx =
        mergeDefCtx stdCtx.defCtx defCtx

    let mergeDefCtx defCtx1 defCtx2 =
        {
            types = Array.append defCtx1.types defCtx2.types
            functions = Array.append defCtx1.functions defCtx2.functions
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

        let maybeDefinedInput = input |> Array.map (Primitive.definedValueToInt) |> Result.combineToArray

        maybeDefinedInput
        |> bind (f >> PrimitiveInt >> DefinedPrimitive >> Ok)

module Primitive = 
    let definedValueToInt v = definedValueToIntInner v 0

    let rec definedValueToIntInner v (recursionCount: int) =
        // prevent infinite recursion while allowing users to pass nested functions
        if recursionCount > 10 then Error (sprintf "Recursion limit reached while trying to convert argument to int for: %A" v) else

        match v with
            | DefinedPrimitive (PrimitiveInt i) -> Ok i
            | DefinedFn (name, fn) ->
                match fn [| |] with
                | Ok(v) -> definedValueToIntInner v (recursionCount + 1)
                | Error e -> Error e

    let readPrimitive (p: string) =
        // ints
        if Seq.forall Char.IsDigit p then p |> int |> PrimitiveInt else

        failwith ("Internal Error: Unable to parse primitive: " + p)

    let isPrim (s: string) = 
        // ints
        Seq.forall Char.IsDigit s

    let getValidatedPrimitiveType primitiveStr =
        match readPrimitive primitiveStr with
            | PrimitiveInt p -> BaseFnType "int"