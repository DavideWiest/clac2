module rec Clac2.Domain

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

type Manipulation = Value array // array of the functions

type Value =
    | Fn of string
    | Primitive of Primitive

type Primitive =
    | PrimitiveInt of int
    // for later
    // | PrimitiveFloat of float
    // | Bool

type TypeDefinition = {
    name: string
    signature: FnType array
}

type FnType =
    | BaseFnType of string
    | Function of FnType array



type DefinedCallableFunction = {
    name: string
    signature: FnType array
    args: string array
    DefinedFn: DefinedManipuation
}

type DefinedManipuation = DefinedValue array

type DefinedValue =
    | DefinedFn of (DefinedValue array -> Result<DefinedValue, string>)
    | DefinedPrimitive of Primitive

type StandardContext = {
    defCtx: DefinitionContext

}

type DefinitionContext = {
    types: string array
    values: string array
}