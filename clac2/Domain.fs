module rec Clac2.Domain

// Files

type MainFileUnparsed = 
    | Interactive of string
    | File of string

type Program = {
    mainFile: MainFile
    secondaryFiles: File array
}

type File = {
    location: string
    content: OrderedFile
}

type MainFile = {
    maybeLocation: string option
    content: OrderedFile
}

// Program

// wrapper, for frontend and utils
type Line =
    | ModuleReference of string 
    | ModuleDeclaration of string 
    | Expression of FreeManipulation // will be sent to stdout
    | Assignment of CallableFunction
    | TypeDefinition of TypeDefinition

type OrderedFile = {
    // moduleDeclaration: string option
    moduleReferences: string array 
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
    | Manipulation of Manipulation

type TypeDefinition = {
    name: string
    signature: FnType array
    loc: ProgramLocation
}

type Signature = FnType array

type FnType =
    | BaseFnType of string
    | Function of FnType array

type Primitive =
    | PrimitiveInt of int
    // for later
    // | PrimitiveFloat of float
    // | Bool

type NestedItems<'a> =
    | NestedItem of 'a
    | NestedArray of NestedItems<'a> array

type NestedItemsArray<'a> = NestedItems<'a> array

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
}

type DefinitionContext = {
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
    locTrace: ProgramLocation list option
}

type FullClacResult<'a> = Result<'a, FullGenericException>