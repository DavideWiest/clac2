module rec Clac2.Language

open Clac2.Domain
open Clac2.Utilities
open FSharp.Core.Result

module Syntax =
    let commentIdentifer = "--"

    let nameIsInvalid (name: string) =
        name 
        |> Seq.exists (fun x -> Seq.contains x " \t\n\r;:()[]{}")
        || isPrimitive name

module Types =
    let supportedTypes = [| "int" |]

    let intType = BaseFnType "int"
    let floatType = BaseFnType "float"

module Files =
    let officialExtensions = [| "clac" |]
    let standardFileLocations = [| "scripts/Standard.clac" |]
    let packageLocation = "clacPackages"
    let toQualifiedFileLoc dir (importName: string) = System.IO.Path.Combine(dir, importName + if Array.exists (fun (ending: string) -> importName.EndsWith(ending)) Files.officialExtensions then "" else Files.officialExtensions[0]) 

module Modules = 
    let reservedFirstOrderModules = [| "Clac", "Std" |]

module BuildIn =
    let baseVars = 
        [|
            ("pi", 3)
            ("e", 3)
        |]
        |> Array.map (fun (k, v) -> k, v |> PrimitiveInt |> DefinedPrimitive)
        |> Array.map (Conversion.FSharpConstantToFn Types.intType)

    let basicArithmeticArgsAndSignature = [| ("n1", Types.intType); ("n2", Types.intType) |]

    let baseFuncs = 
        [|
            ("add", (+))
            ("subtract", (-))
            ("mul", (*))
            ("div", (/))
        |]
        |> Array.map (fun (k, f) -> k, fun input -> Array.reduce f input)
        |> Array.map (fun (k, f) -> k, fun input -> Conversion.fnTypeToIntAdapter f input 2)
        |> Array.map (Conversion.fSharpFunctionToFn basicArithmeticArgsAndSignature Types.intType)

module StandardContext =
    let buildStandardContext (baseFuncs: DefinedCallableFunction array) (supportedTypes: string array) commentIdentifier =
        {
            defCtx = {
                types = supportedTypes
                functions = baseFuncs |> Array.map (fun x -> x.name)
            }
            definedCtx = {
                functions = baseFuncs
            }
            commentIdentifier = commentIdentifier
        }

module Conversion = 
    let FSharpConstantToFn constantType (x: string * DefinedValue) =
        {
            name = x |> fst
            signature = [| constantType |]
            args = [| |]
            DefinedFn = fun _ -> x |> snd |> Ok
        }

    let fSharpFunctionToFn (typedArgs: (string * FnType) array) (returnType: FnType) (nameAndFn: string * DefinedFn) =
        {
            name = nameAndFn |> fst
            signature = typedArgs |> Array.map snd |> Array.append [| returnType |]
            args = typedArgs |> Array.map fst
            DefinedFn = nameAndFn |> snd
        }

    let fnTypeToIntAdapter (f: int array -> int) (input: DefinedValue array) nArgs : Result<DefinedValue, string> =
        if input.Length <> nArgs then Error "Internal Error: Wrong number of arguments for fnTypeToIntAdapter" else

        input
        |> Array.map (definedValueToPrimitive)
        |> combineResultsToArray
        |> bind (f >> PrimitiveInt >> DefinedPrimitive >> Ok)
