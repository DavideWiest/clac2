module rec Clac2.Core.Domain

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

type fileDependencyMap = Map<string option, DefinitionContext>

// Program

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
    manip: Manipulation
    loc: ProgramLocation
    fnOptions: FnOptions
}

type FnOptions = {
    fixation: OperatorFixation
    noMemo: bool
}

type OperatorFixation = Prefix | Infix | Postfix

type FreeManipulation = {
    manip: Manipulation
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

// Defined

type DefinedCallableFunction = {
    name: string
    signature: FnType array
    args: string array
    DefinedFn: DefinedFn
    fnOptions: FnOptions
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

