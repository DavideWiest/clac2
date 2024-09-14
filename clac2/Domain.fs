module rec Clac2.Domain

// Files

type MainFileUnparsed = 
    | Interactive of string
    | File of string

type Program = {
    mainFile: MainFile
    otherLocalFiles: File array
    standardFiles: File array
}

type File = {
    location: string
    lines: Line array
}

type MainFile = {
    maybeLocation: string option
    lines: Line array
}

// Program

type Line =
    | Expression of Manipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition
    // for later
    | ModuleReference of string

type OrderedFile = {
    moduleName: WithLine<string> option
    expressions: WithLine<Manipulation> array
    assignments: WithLine<CallableFunction> array
    typeDefinitions: WithLine<TypeDefinition> array
}

type WithLine<'a> = {
    line: Line
    value: 'a
}

// Language

type CallableFunction = {
    name: string
    signature: FnType array
    args: string array
    // for later
    //innerAssignments: CallableFunction array
    fn: Manipulation
}

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
    defCtx: DefinedSymbols
    definedCtx: DefinedContext
    commentIdentifier: string
}

type DefinedSymbols = {
    types: string array
    functions: string array
}

type DefinedContext = {
    functions: DefinedCallableFunction array
}

// Errors 

type Trace = {
    exc: GenericException
    trace: ExceptionLocation list
}

type GenericException = {
    message: string
    exceptionLocation: ExceptionLocation
}

type ExceptionLocation = {
    location: string option
    line: int
}

type ClacResult<'a> = Result<'a, GenericException>