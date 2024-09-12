module rec Clac2.Domain

// Base

type Program = {
    loadedFiles: File array
    localFiles: File array
}

type File = {
    location: string
    lines: Line array
}

type Line =
    | Expression of Manipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition

type CallableFunction = {
    name: string
    signature: FnType array
    args: string array
    // for later
    //innerAssignments: CallableFunction array
    fn: Manipulation
}

// unnormalized manipulation 
type Manipulation = Reference array // array of the functions

type Reference =
    | Fn of string
    // for later 
    // | Manipulation of Manipulation

type TypeDefinition = {
    name: string
    signature: FnType array
}

type FnType =
    | BaseFnType of string
    | Function of FnType array

type Primitive =
    | PrimitiveInt of int
    // for later
    // | PrimitiveFloat of float
    // | Bool

// Defined

type DefinedCallableFunction = {
    name: string
    signature: FnType array
    args: string array
    DefinedFn: DefinedFn
}

type DefinedValue =
    | DefinedPrimitive of Primitive
    | DefinedFn of string * DefinedFn

type DefinedFn = DefinedValue array -> Result<DefinedValue, string>

// Context

type StandardContext = {
    defCtx: DefinitionContext
    definedCtx: DefinedContext
    commentIdentifier: string
}

type DefinitionContext = {
    types: string array
    functions: string array
}

type DefinedContext = {
    functions: DefinedCallableFunction array
}