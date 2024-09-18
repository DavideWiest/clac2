module rec Clac2.FrontEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.Language
open Clac2.DomainUtilities
open FSharp.Core.Result

type UnparsedLine =
    | UnparsedExpression of UnparsedManipulation
    | UnparsedAssignment of UnparsedCallableFunction
    | UnparsedTypeDefinition of UnparsedTypeDefinition

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

let parse fileLoc (standardContext: StandardContext) (input: string) : ClacResult<Line array> =
    let defCtx = standardContext.defCtx

    input
    |> trimSplit [| '\n'; ';' |]
    |> Array.filter (fun x -> x <> "")
    |> Array.filter (fun x -> not (x.StartsWith standardContext.commentIdentifier))
    |> Array.map ToUnparsedLine
    |> combineResultsToArray

    |> bind (fun preParsedLines ->
        let customTypes = preParsedLines |> Array.choose (fun x -> match x with | UnparsedTypeDefinition t -> Some t.name | _ -> None)
        let customAssignments = preParsedLines |> Array.choose (fun x -> match x with | UnparsedAssignment f -> Some f.name | _ -> None)
        let definitionContext = { 
            defCtx with types = Array.concat [customTypes; defCtx.types]; functions = Array.concat [customAssignments; defCtx.functions] 
        }
        
        let maybeFunctionDuplicates = hasDuplicatesByReturning definitionContext.functions id
        if maybeFunctionDuplicates.Length > 0 then ClacError ("Duplicate function definitions: " + (maybeFunctionDuplicates |> String.concat ", ")) else

        let maybeTypeDuplicates = hasDuplicatesByReturning definitionContext.types id
        if maybeTypeDuplicates.Length > 0 then ClacError ("Duplicate type definitions: " + (maybeTypeDuplicates |> String.concat ", ")) else

        preParsedLines
        |> Array.map (ParseLine fileLoc definitionContext)
        |> combineResultsToArray
    )

let ToUnparsedLine (line: string) : ClacResult<UnparsedLine> =
    if line.StartsWith "let " then
        line |> ToUnparsedCallableFunction |> map UnparsedAssignment
    else if line.StartsWith "type " then
        line |> ToUnparsedTypeDefinition |> map UnparsedTypeDefinition
    else
        line |> ToUnparsedManipulation |> UnparsedExpression |> Ok

let ToUnparsedCallableFunction (line: string) : ClacResult<UnparsedCallableFunction> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 6 then ClacError "Assignment missing parts. Missing space?" else

    let firstColon = parts |> Array.findIndex (fun x -> x = ":")
    let firstEqual = parts |> Array.findIndex (fun x -> x = "=")

    if firstColon = -1 || firstEqual = -1 then ClacError "Assignment missing a colon or an equals." else
    if firstColon > firstEqual then ClacError "Assignment is missing a colon before the assignment." else
    if firstColon = firstEqual - 1 then ClacError "Assignment missing type signature between colon and equals." else
    if parts.Length = firstEqual + 1 then ClacError "Assignment missing function body." else

    let nameAndArgs = parts[1..firstColon-1]
    let name , args = nameAndArgs[0], nameAndArgs[1..]
    if Syntax.nameIsInvalid name then ClacError ("Invalid function name: " + name) else
    let signatureParts = parts[firstColon+1..firstEqual-1]
    if (signatureParts |> Array.length) - 1 <> (args |> Array.length) then ClacError "Function signature does not match number of arguments." else
    if args |> Array.map Syntax.nameIsInvalid |> Array.exists id then ClacError "Invalid argument name." else

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

let ToUnparsedTypeDefinition (line: string) : ClacResult<UnparsedTypeDefinition> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 3 then ClacError "Type definition missing parts. Missing space?" else

    let maybeFirstColon = parts |> Array.tryFindIndex (fun x -> x = ":")
    if maybeFirstColon = None then ClacError "Type definition missing a colon separated by spaces." else

    let name = parts[1]
    if Syntax.nameIsInvalid name then ClacError ("Invalid type name: " + name) else

    let signature = parts[maybeFirstColon.Value+1..] |> String.concat " "

    {
        name = name
        unparsedSignature = signature
    } 
    |> Ok

let ParseLine fileLoc (definitionContext: DefinedSymbols) (line: UnparsedLine) : ClacResult<Line> =
    match line with
    | UnparsedExpression m -> m |> ParseManipulation definitionContext |> map Expression
    | UnparsedAssignment f -> f |> ParseCallableFunction fileLoc definitionContext |> map Assignment
    | UnparsedTypeDefinition t -> t |> ParseTypeDefinition definitionContext |> map TypeDefinition

let ParseCallableFunction fileLoc (definitionContext: DefinedSymbols) (f: UnparsedCallableFunction) : ClacResult<CallableFunction> =
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
            fileLocation = fileLoc
        } |> Ok
    | errTuple -> errTuple |> joinErrorTuple |> Error

let ParseManipulation (definitionContext: DefinedSymbols) (m: UnparsedManipulation) : ClacResult<Manipulation> =
    m
    |> Array.map (fun x ->
        if Array.contains x definitionContext.functions || isPrimitive x then
            x |> Fn |> Ok
        else
            ClacError ("Unknown function: " + x)
    )
    |> combineClacResultsToArray

let ParseTypeDefinition (definitionContext: DefinedSymbols) (t: UnparsedTypeDefinition) : ClacResult<TypeDefinition> =
    t.unparsedSignature 
    |> trimSplit [| ' ' |] 
    |> Array.map (stringToType definitionContext) 
    |> combineClacResultsToArray
    |> map (fun signature -> { name = t.name; signature = signature })

let stringToType (definitionContext: DefinedSymbols) (s: string) : ClacResult<FnType> =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok |> toClacResult
    | _ -> ClacError ("Unknown type: " + s)
