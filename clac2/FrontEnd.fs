module rec Clac2.FrontEnd

open Clac2.Domain
open Clac2.Utilities
open System
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

let parse (standardContext: StandardContext) (input: string) : Result<Line array, string> =
    let defCtx = standardContext.defCtx

    let maybePreParsedLines = 
        input
        |> trimSplit [| '\n'; ';' |]
        |> Array.filter (fun x -> x <> "")
        |> Array.filter (fun x -> not (x.StartsWith standardContext.commentIdentifier))
        |> Array.map ToUnparsedLine
        |> combineResultsToArray

    match maybePreParsedLines with
    | Error e -> Error e
    | Ok preParsedLines ->

        let customTypes = preParsedLines |> Array.choose (fun x -> match x with | UnparsedTypeDefinition t -> Some t.name | _ -> None)
        let customAssignments = preParsedLines |> Array.choose (fun x -> match x with | UnparsedAssignment f -> Some f.name | _ -> None)
        let definitionContext = { 
            defCtx with types = Array.concat [customTypes; defCtx.types]; functions = Array.concat [customAssignments; defCtx.functions] 
        }
        
        let maybeFunctionDuplicates = hasDuplicatesByReturning definitionContext.functions id
        if maybeFunctionDuplicates.Length > 0 then Error ("Duplicate function definitions: " + (maybeFunctionDuplicates |> String.concat ", ")) else

        let maybeTypeDuplicates = hasDuplicatesByReturning definitionContext.types id
        if maybeTypeDuplicates.Length > 0 then Error ("Duplicate type definitions: " + (maybeTypeDuplicates |> String.concat ", ")) else

        preParsedLines
        |> Array.map (ParseLine definitionContext)
        |> combineResultsToArray

let ToUnparsedLine (line: string) : Result<UnparsedLine, string> =
    if line.StartsWith "let " then
        line |> ToUnparsedCallableFunction |> map UnparsedAssignment
    else if line.StartsWith "type " then
        line |> ToUnparsedTypeDefinition |> map UnparsedTypeDefinition
    else
        line |> ToUnparsedManipulation |> map UnparsedExpression

let ToUnparsedCallableFunction (line: string) : Result<UnparsedCallableFunction, string> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 6 then Error "Assignment missing parts. Missing space?" else

    let firstColon = parts |> Array.findIndex (fun x -> x = ":")
    let firstEqual = parts |> Array.findIndex (fun x -> x = "=")

    if firstColon = -1 || firstEqual = -1 then Error "Assignment missing a colon or an equals." else
    if firstColon > firstEqual then Error "Assignment is missing a colon before the assignment." else
    if firstColon = firstEqual - 1 then Error "Assignment missing type signature between colon and equals." else

    let nameAndArgs = parts[1..firstColon-1]
    let name , args = nameAndArgs[0], nameAndArgs[1..]
    if not (nameIsValid name) then Error ("Invalid function name: " + name) else
    let signatureParts = parts[firstColon+1..firstEqual-1]
    if (signatureParts |> Array.length) - 1 <> (args |> Array.length) then Error "Function signature does not match number of arguments." else
    if args |> Array.map nameIsValid |> Array.exists (id >> not) then Error "Invalid argument name." else

    let signature = signatureParts |> String.concat " "
    let maybeFnBody = parts[firstEqual+1..] |> String.concat " " |> ToUnparsedManipulation
    
    match maybeFnBody with
    | Error e -> Error ("Error parsing function body: " + e)
    | Ok fnBody ->
        {
            name = name
            unparsedSignature = signature
            args = args
            fn = fnBody
        } 
        |> Ok

// does not support precedence/nesting
let ToUnparsedManipulation (line: string) : Result<UnparsedManipulation, string> =
    let parts = line |> trimSplit [| ' ' |]
    
    parts |> Ok

let ToUnparsedTypeDefinition (line: string) : Result<UnparsedTypeDefinition, string> =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 3 then Error "Type definition missing parts. Missing space?" else

    let maybeFirstColon = parts |> Array.tryFindIndex (fun x -> x = ":")
    if maybeFirstColon = None then Error "Type definition missing a colon separated by spaces." else

    let name = parts[1]
    if not (nameIsValid name) then Error ("Invalid type name: " + name) else

    let signature = parts[maybeFirstColon.Value+1..] |> String.concat " "

    {
        name = name
        unparsedSignature = signature
    } |> Ok

let ParseLine (definitionContext: DefinitionContext) (line: UnparsedLine) : Result<Line, string> =
    match line with
    | UnparsedExpression m -> m |> ParseManipulation definitionContext |> map Expression
    | UnparsedAssignment f -> f |> ParseCallableFunction definitionContext |> map Assignment
    | UnparsedTypeDefinition t -> t |> ParseTypeDefinition definitionContext |> map TypeDefinition

let ParseCallableFunction (definitionContext: DefinitionContext) (f: UnparsedCallableFunction) : Result<CallableFunction, string> =
    let maybeSignature = 
        f.unparsedSignature 
        |> trimSplit [| ' ' |] 
        |> Array.map (stringToType definitionContext) 
        |> combineResultsToArray

    let maybeFn = f.fn |> ParseManipulation {definitionContext with functions = Array.concat [definitionContext.functions; f.args]}

    match (maybeSignature, maybeFn) with
    | (Ok signature, Ok fn) ->
        {
            name = f.name
            signature = signature
            args = f.args
            fn = fn
        } |> Ok
    | errTuple -> Error (errTuple |> joinErrorTuple |> sprintf "Error parsing function: %s")

let ParseManipulation (definitionContext: DefinitionContext) (m: UnparsedManipulation) : Result<Manipulation, string> =
    m
    |> Array.map (fun x ->
        if Array.contains x definitionContext.functions then
            x |> Fn |> Ok
        else if isPrimitive x then
            x |> ToPrimitive |> map Primitive
        else
            Error ("Unknown function: " + x)
    )
    |> combineResultsToArray

let isPrimitive (s: string) =
    s 
    |> Seq.forall (fun x -> Char.IsDigit x) // for later: || x = '.')

let ToPrimitive (s: string) =
    s |> int |> PrimitiveInt |> Ok

let ParseTypeDefinition (definitionContext: DefinitionContext) (t: UnparsedTypeDefinition) : Result<TypeDefinition, string> =
    t.unparsedSignature 
    |> trimSplit [| ' ' |] 
    |> Array.map (stringToType definitionContext) 
    |> combineResultsToArray
    |> map (fun signature -> { name = t.name; signature = signature })

let stringToType (definitionContext: DefinitionContext) (s: string) : Result<FnType, string> =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok
    | _ -> Error ("Unknown type: " + s)
