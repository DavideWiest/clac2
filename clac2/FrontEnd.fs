module rec Clac2.FrontEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.Language
open Clac2.DomainUtilities
open Clac2.Language
open FSharp.Core.Result

type UnparsedLine =
    | UnparsedExpression of UnparsedManipulation
    | UnparsedAssignment of UnparsedCallableFunction
    | UnparsedTypeDefinition of UnparsedTypeDefinition
    | UnparsedModuleDeclaration of string
    | UnparsedModuleReference of string

type UnparsedCallableFunction = {
    name: string
    unparsedSignature: string
    args: string array
    // for later
    //innerAssignments: UnparsedCallableFunction array
    fn: UnparsedManipulation
}

type UnparsedManipulation = string array // input -> output

type UnparsedTypeDefinition = {
    name: string
    unparsedSignature: string
}

let parseFull fileLoc (stdCtx: StandardContext) (input: string) : FullClacResult<OrderedFile> =
    preparse input |> toFullResult fileLoc |> bind (parseFullResult fileLoc stdCtx)

let preparse (input: string) : IntermediateClacResult<(int * UnparsedLine) array> =
    input
    |> trimSplit [| '\n' |]
    |> Array.mapi (fun i x -> (i, x))
    |> trimSplitIndexedArray [| ';' |]
    |> Array.filter (fun x -> (snd x) <> "")
    |> Array.filter (fun x -> not ((snd x).StartsWith Syntax.commentIdentifer))
    |> Array.map (unpackTuple ToUnparsedLine)
    |> combineResultsToArray

let parseFullResult fileLoc stdCtx preParsedLines = parse fileLoc stdCtx preParsedLines |> toFullResult fileLoc 

let parse fileLoc stdCtx (preParsedLines: (int * UnparsedLine) array) =    
    let localDefCtx = extractCustomDefinitions preParsedLines
    let definitionContext = { 
        stdCtx.defCtx with types = Array.concat [localDefCtx.types; stdCtx.defCtx.types]; functions = Array.concat [localDefCtx.functions; stdCtx.defCtx.functions] 
    }
    
    let maybeFunctionDuplicate = hasDuplicatesByReturningFirstWithIndex definitionContext.functions id
    if maybeFunctionDuplicate.IsSome then IntermediateExcFromParts ("Duplicate function definition for: " + (maybeFunctionDuplicate.Value |> snd)) (maybeFunctionDuplicate.Value |> fst) else

    let maybeTypeDuplicate = hasDuplicatesByReturningFirstWithIndex definitionContext.types id
    if maybeTypeDuplicate.IsSome then IntermediateExcFromParts ("Duplicate type definition for: " + (maybeTypeDuplicate.Value |> snd)) (maybeTypeDuplicate.Value |> fst) else

    printfn "Preparsed lines of %s %A" (fileLoc |> Option.defaultValue "interactive") preParsedLines

    preParsedLines
    |> Array.map (fun (i, l) -> i, ParseLine (buildLoc fileLoc i) definitionContext l)
    |> Array.map (fun (i, r) -> toIntermediateResult i r)
    |> combineResultsToArray
    |> map toOrderedFile

let extractCustomDefinitions preParsedLines  =
    let customTypes = preParsedLines |> Array.choose (fun (i, x) -> match x with | UnparsedTypeDefinition t -> Some t.name | _ -> None)
    let customAssignments = preParsedLines |> Array.choose (fun (i, x) -> match x with | UnparsedAssignment f -> Some f.name | _ -> None)
    { types = customTypes; functions = customAssignments }

let ToUnparsedLine (i: int) (line: string) : IntermediateClacResult<int * UnparsedLine> =
    line |> ToUnparsedLineInner |> toIntermediateResult i |> map (fun x -> (i, x))

let ToUnparsedLineInner (line: string) : GenericResult<UnparsedLine> =
    if line.StartsWith "let " then
        line |> ToUnparsedCallableFunction |> map UnparsedAssignment
    else if line.StartsWith "type " then
        line |> ToUnparsedTypeDefinition |> map UnparsedTypeDefinition
    else if line.StartsWith "module " then
        line.Substring(7).Trim() |> UnparsedModuleDeclaration |> Ok
    else if line.StartsWith "open " then
        line.Substring(5).Trim() |> UnparsedModuleReference |> Ok
    else
        line |> ToUnparsedManipulation |> UnparsedExpression |> Ok

let ToUnparsedCallableFunction (line: string) : GenericResult<UnparsedCallableFunction> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 6 then GenExcError "Assignment missing parts. Missing space?" else

    let firstColon = parts |> Array.findIndex (fun x -> x = ":")
    let firstEqual = parts |> Array.findIndex (fun x -> x = "=")

    if firstColon = -1 || firstEqual = -1 then GenExcError "Assignment missing a colon or an equals." else
    if firstColon > firstEqual then GenExcError "Assignment is missing a colon before the assignment." else
    if firstColon = firstEqual - 1 then GenExcError "Assignment missing type signature between colon and equals." else
    if parts.Length = firstEqual + 1 then GenExcError "Assignment missing function body." else

    let nameAndArgs = parts[1..firstColon-1]
    let name , args = nameAndArgs[0], nameAndArgs[1..]
    if Syntax.nameIsInvalid name then GenExcError ("Invalid function name: " + name) else
    let signatureParts = parts[firstColon+1..firstEqual-1]
    if (signatureParts |> Array.length) - 1 <> (args |> Array.length) then GenExcError "Function signature does not match number of arguments." else
    if args |> Array.map Syntax.nameIsInvalid |> Array.exists id then GenExcError "Invalid argument name." else

    let signature = signatureParts |> String.concat " "
    let fnBody = parts[firstEqual+1..] |> String.concat " " |> ToUnparsedManipulation

    {
        name = name
        unparsedSignature = signature
        args = args
        fn = fnBody
    } 
    |> Ok

// does not support precedence/nesting
let ToUnparsedManipulation (line: string) = line |> trimSplit [| ' ' |]

let ToUnparsedTypeDefinition (line: string) : GenericResult<UnparsedTypeDefinition> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 3 then GenExcError "Type definition missing parts. Missing space?" else

    let maybeFirstColon = parts |> Array.tryFindIndex (fun x -> x = ":")
    if maybeFirstColon = None then GenExcError "Type definition missing a colon separated by spaces." else

    let name = parts[1]
    if Syntax.nameIsInvalid name then GenExcError ("Invalid type name: " + name) else

    let signature = parts[maybeFirstColon.Value+1..] |> String.concat " "

    {
        name = name
        unparsedSignature = signature
    } 
    |> Ok

let ParseLine loc (definitionContext: DefinitionContext) (line: UnparsedLine) : GenericResult<Line> =
    match line with
    | UnparsedExpression m -> m |> ParseManipulation definitionContext |> map (fun manipulation -> Expression { manipulation = manipulation; loc = loc })
    | UnparsedAssignment f -> f |> ParseCallableFunction loc definitionContext |> map Assignment
    | UnparsedTypeDefinition t -> t |> ParseTypeDefinition loc definitionContext |> map TypeDefinition
    | UnparsedModuleDeclaration m -> parseModuleDeclaration loc m
    | UnparsedModuleReference m -> parseModuleReference loc m

let ParseCallableFunction loc (definitionContext: DefinitionContext) (f: UnparsedCallableFunction) : GenericResult<CallableFunction> =
    let maybeSignature = 
        f.unparsedSignature 
        |> trimSplit [| ' ' |] 
        |> Array.map (stringToType definitionContext) 
        |> combineClacResultsToArray 

    let maybeFn = f.fn |> ParseManipulation {definitionContext with functions = Array.concat [definitionContext.functions; f.args]}

    match (maybeSignature, maybeFn) with
    | (Ok signature, Ok fn) ->
        {
            name = f.name
            signature = signature
            args = f.args
            fn = fn
            loc = loc
        } |> Ok
    | errTuple -> errTuple |> joinErrorTuple |> Error

let ParseManipulation (definitionContext: DefinitionContext) (m: UnparsedManipulation) : GenericResult<Manipulation> =
    m
    |> Array.map (fun x ->
        if Array.contains x definitionContext.functions || isPrimitive x then
            x |> Fn |> Ok
        else
            GenExcError ("Unknown function: " + x)
    )
    |> combineClacResultsToArray

let ParseTypeDefinition loc (definitionContext: DefinitionContext) (t: UnparsedTypeDefinition) : GenericResult<TypeDefinition> =
    t.unparsedSignature 
    |> trimSplit [| ' ' |] 
    |> Array.map (stringToType definitionContext) 
    |> combineClacResultsToArray
    |> map (fun signature -> { name = t.name; signature = signature; loc=loc })

let stringToType (definitionContext: DefinitionContext) (s: string) : GenericResult<FnType> =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok |> toGenericResult
    | _ -> GenExcError ("Unknown type: " + s)

let parseModuleDeclaration loc (line: string) : GenericResult<Line> =
    match loc.fileLocation with
    | None -> GenExcError "Module declaration outside of file."
    | Some fileLoc -> 
        let dir = System.IO.Path.GetDirectoryName fileLoc
        
        // for now, module and files names must match
        if line |> Files.toQualifiedFileLoc dir <> fileLoc then GenExcError "Module declaration does not match file location." else

        line |> Files.toQualifiedFileLoc dir |> ModuleDeclaration |> Ok

let parseModuleReference loc (line: string) : GenericResult<Line> =
    let dir = getDirOfReference loc.fileLocation
    line |> Files.toQualifiedFileLoc dir |> ModuleReference |> Ok

let getDirOfReference fileLoc =
    match fileLoc with
    | Some fileLoc -> System.IO.Path.GetDirectoryName fileLoc
    | None -> Files.packageLocation

let toOrderedFile (lines: Line array) : OrderedFile =
    let moduleName = lines |> Array.tryHead |> Option.bind (fun x -> match x with | ModuleDeclaration m -> Some m | _ -> None)
    let moduleReferences = lines |> Array.choose (fun x -> match x with | ModuleReference m -> Some m | _ -> None)
    let expressions = lines |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)
    let assignments = lines |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
    let typeDefinitions = lines |> Array.choose (fun x -> match x with | TypeDefinition t -> Some t | _ -> None)

    {
        moduleDeclaration = moduleName
        moduleReferences = moduleReferences
        expressions = expressions
        assignments = assignments
        typeDefinitions = typeDefinitions
    }