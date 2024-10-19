module rec Clac2.FrontEnd.Domain

open Clac2.Core.Domain

type UnparsedLine =
    | UnparsedExpression of NestedItemsArray<string>
    | UnparsedAssignment of UnparsedCallableFunction
    | UnparsedTypeDefinition of UnparsedTypeDefinition
    | UnparsedModuleDeclaration of string
    | UnparsedModuleReference of string

type Line =
    | ModuleReference of string 
    | ModuleDeclaration of string 
    | Expression of FreeManipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition

type UnparsedCallableFunction = {
    name: string
    unparsedSignature: NestedItemsArray<string>
    args: string array
    fn: NestedItemsArray<string>
    fnOptions: FnOptions
    // for later
    //innerAssignments: UnparsedCallableFunction array
}

type UnparsedTypeDefinition = {
    name: string
    unparsedSignature: NestedItemsArray<string>
}

type NestedItemsArray<'a> = NestedItems<'a> array

type NestedItems<'a> =
    | NestedItem of 'a
    | NestedArray of NestedItems<'a> array
