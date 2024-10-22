module rec Clac2.Core.Context

open Clac2.Core.Domain


type ScopeCtx = {
    types: string array
    functions: string array
}

type fileDependencyMap = Map<string option, ScopeCtx>

module ScopeCtx =
    let init types functions = { types = types; functions = functions |> Array.map (fun x -> x.name) }

    let getDefCtxWithStdCtxFromMap defCtx (depMap: fileDependencyMap) fileLoc = mergeDefCtx defCtx depMap[fileLoc]

    let mergeDefCtx defCtx1 defCtx2 =
        {
            types = Array.append defCtx1.types defCtx2.types
            functions = Array.append defCtx1.functions defCtx2.functions
        }

type CallableCtx = {
    functions: DefinedCallableFunction array
}

module CallableCtx =
    let init (functions: DefinedCallableFunction array) = { functions = functions }
