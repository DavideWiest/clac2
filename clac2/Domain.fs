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
    lines: OrderedFile
}

type MainFile = {
    maybeLocation: string option
    content: OrderedFile
}

// Program

// wrapper, for frontend and utils
type Line =
    | Expression of FreeManipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition
    // for later
    | ModuleReference of string array
    | ModuleDeclaration of string array

type OrderedFile = {
    moduleDeclaration: string array option
    moduleReferences: string array array
    expressions: FreeManipulation array
    assignments: CallableFunction array
    typeDefinitions: TypeDefinition array
}

type ProgramLocation = {
    fileLocation: string option
    lineLocation: int
}

// Language

type CallableFunction = {
    name: string
    signature: FnType array
    args: string array
    // for later
    //innerAssignments: CallableFunction array
    fn: Manipulation
    loc: ProgramLocation
}

type FreeManipulation = {
    manipulation: Manipulation
    loc: ProgramLocation
}

type Manipulation = Reference array // array of the functions

type Reference =
    | Fn of string
    // for later 
    // is this of any use?
    // | Manipulation of Manipulation

type TypeDefinition = {
    name: string
    signature: FnType array
    loc: ProgramLocation
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

type GenericException = {
    message: string
}

type GenericResult<'a> = Result<'a, GenericException>

type IntermediateException = {
    genExc: GenericException
    line: int option
}

type IntermediateClacResult<'a> = Result<'a, IntermediateException>

type FullGenericException = {
    genExcWithLine: IntermediateException
    fileLocation: string option
}

type FullClacResult<'a> = Result<'a, FullGenericException>