module rec Clac2.FrontEnd.Domain

open Clac2.Core.Domain
open Clac2.FrontEnd.NestedItems

type UnparsedLine =
    | UnparsedExpression of NestedItemArray<string>
    | UnparsedAssignment of UnparsedCallableFunction
    | UnparsedTypeDefinition of UnparsedTypeDefinition
    | UnparsedModuleReference of string

type Line =
    | ModuleReference of string 
    | Expression of FreeManipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition

type UnparsedCallableFunction = {
    name: string
    unparsedSignature: NestedItemArray<string>
    args: string array
    fn: NestedItemArray<string>
    fnOptions: FnOptions
    // for later
    //innerAssignments: UnparsedCallableFunction array
}

type UnparsedTypeDefinition = {
    name: string
    unparsedSignature: NestedItemArray<string>
}
