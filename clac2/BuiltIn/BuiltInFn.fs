module rec Clac2.BuiltIn.BuiltInFn

open Clac2.Core.Domain
open FSharp.Core.Result


type BuiltInFn<'a, 'b> = {
    name: string
    signature: FnType array
    args: string array
    fnOptions: FnOptions
    conversionFn: DefinedValue array -> Result<'a, string>
    innerFn: 'a -> Result<'b, string>
    reconversionFn: 'b -> DefinedValue
}

module BuiltInFn =
    let initThroughAdapter builtInFn =
        let fn = fnAdapter builtInFn.name (builtInFn.signature.Length - 1) builtInFn.conversionFn builtInFn.innerFn builtInFn.reconversionFn
        DefinedCallableFunction.init builtInFn.name builtInFn.signature builtInFn.args fn builtInFn.fnOptions

    let rec fnAdapter name nArgs conversionFn innerFn reconversionFn (input: DefinedValue array)  =
            if input.Length < nArgs then 
                (name + sprintf " (%i/%i args)" input.Length nArgs, fun args -> fnAdapter name nArgs conversionFn innerFn reconversionFn (Array.append input args))
                |> DefinedFn 
                |> Ok
            else

            input
            |> conversionFn
            |> bind innerFn
            |> map reconversionFn

module DefinedCallableFunction =
    let init name (signature: FnType array) (args: string array) fn fnOptions =
        if args.Length <> signature.Length - 1 then failwithf "Signature length mismatch for function %s" name else

        {
            name = name
            signature = signature
            args = args
            DefinedFn = fn
            fnOptions = fnOptions
        }

let genericFnOptions = { fixation = Prefix; noMemo = false }