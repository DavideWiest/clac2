module rec Clac2.FrontEnd

open Clac2.Domain
open Clac2.Utilities
open Clac2.Language
open Clac2.DomainUtilities
open FSharp.Core.Result
open System.Collections.Generic

type UnparsedLine =
    | UnparsedExpression of NestedItemsArray<string>
    | UnparsedAssignment of UnparsedCallableFunction
    | UnparsedTypeDefinition of UnparsedTypeDefinition
    | UnparsedModuleDeclaration of string
    | UnparsedModuleReference of string

type UnparsedCallableFunction = {
    name: string
    unparsedSignature: NestedItemsArray<string>
    args: string array
    fn: NestedItemsArray<string>
    // for later
    //innerAssignments: UnparsedCallableFunction array
    fnOptions: FnOptions
}

type UnparsedTypeDefinition = {
    name: string
    unparsedSignature: NestedItemsArray<string>
}

let parseFull fileLoc stdCtx input =
    preparse input |> toFullResult fileLoc |> bind (parseFullResult fileLoc stdCtx.defCtx)

let preparse input =
    input
    |> Parsing.trimSplitSimple [| '\n' |]
    |> Array.mapi (fun i x -> (i, x))
    |> Array.map (fun (i, x) -> i, String.cutOffAt x Language.Syntax.commentIdentifer)
    |> Array.filter (fun x -> not ((snd x).StartsWith Syntax.commentIdentifer))
    |> Parsing.trimSplitIndexedArray [| ';' |]
    |> Array.filter (fun x -> (snd x) <> "")
    |> Array.map (applyUnpacked ToUnparsedLine)
    |> Result.combineToArray

let parseFullResult fileLoc defCtx preParsedLines = parse fileLoc defCtx preParsedLines |> toFullResult fileLoc 

let parse fileLoc defCtx preParsedLines =  
    let (localTypesWithLines, localFunctionsWithLines) = extractCustomDefinitionsWithLine preParsedLines

    let duplicationError arr source symbolName = 
        let maybeDup = chooseHigherOccurenceElements 1 arr source |> Array.tryHead
        match maybeDup with
        | Some (dupLine, dupName) -> IntermediateExcFromParts ("Duplicate " + symbolName + " definition for: " + dupName) dupLine |> Some
        | None -> None

    let dupFnResult = duplicationError localFunctionsWithLines defCtx.functions "function"
    if dupFnResult.IsSome then Error dupFnResult.Value else
    let dupTypeResult = duplicationError localTypesWithLines defCtx.types "type"
    if dupTypeResult.IsSome then Error dupTypeResult.Value else

    preParsedLines
    |> Array.map (fun (i, l) -> i, ParseLine (buildLoc fileLoc i) defCtx l)
    |> Array.map (fun (i, r) -> toIntermediateResult i r)
    |> Result.combineToArray
    |> map toOrderedFile

let extractCustomDefinitions preParsedLines  =
    let (localTypesWithLine, localFunctionWithLine) = preParsedLines |> extractCustomDefinitionsWithLine
    {
        types = localTypesWithLine |> Array.map snd
        functions = localFunctionWithLine |> Array.map snd
    }

let extractCustomDefinitionsWithLine preParsedLines =
    let localTypes = preParsedLines |> Array.choose (fun (i, x) -> match x with | UnparsedTypeDefinition t -> Some (i, t.name) | _ -> None)
    let localFunctions = preParsedLines |> Array.choose (fun (i, x) -> match x with | UnparsedAssignment f -> Some (i, f.name) | _ -> None)
    
    localTypes, localFunctions

let ToUnparsedLine i line =
    line |> ToUnparsedLineInner |> toIntermediateResult i |> map (fun x -> (i, x))

let ToUnparsedLineInner line =
    if line.StartsWith "module " then
        line.Substring(7).Trim() |> UnparsedModuleDeclaration |> Ok
    else if line.StartsWith "open " then
        line.Substring(5).Trim() |> UnparsedModuleReference |> Ok
    else if line.StartsWith "let " then
        line |> ToUnparsedCallableFunction |> map UnparsedAssignment
    else if line.StartsWith "type " then
        line |> ToUnparsedTypeDefinition |> map UnparsedTypeDefinition
    else
        line |> ToUnparsedManipulation |> map UnparsedExpression

let ToUnparsedCallableFunction line =
    let partsBefore: string array = line |> Parsing.trimSplit [| ' ' |]

    if partsBefore.Length < 2 then GenExcError "Assignment missing parts. Missing space?" else
    
    let fnOptionStrings, (parts: string array) = separateFnOptions [] partsBefore[1..]

    if List.contains "infix" fnOptionStrings && List.contains "postfix" fnOptionStrings then GenExcError "Function cannot be both infix and postfix." else 
    if parts.Length < 5 then GenExcError "Assignment missing parts. Missing space?" else

    let firstColonI = parts |> Array.findIndex (fun x -> x = ":")
    let firstEqualI = parts |> Array.findIndex (fun x -> x = "=")

    if firstColonI = -1 || firstEqualI = -1 then GenExcError "Assignment missing a colon or an equals." else
    if firstColonI > firstEqualI then GenExcError "Assignment is missing a colon before the assignment." else
    if firstColonI = firstEqualI - 1 then GenExcError "Assignment missing type signature between colon and equals." else
    if parts.Length = firstEqualI + 1 then GenExcError "Assignment missing function body." else

    let nameAndArgs = parts[..firstColonI-1]
    let name , args = nameAndArgs[0], nameAndArgs[1..]
    if Syntax.nameIsInvalid name then GenExcError ("Invalid function name: " + name) else

    extractFullSignature parts[firstColonI+1..firstEqualI-1]
    |> bind (fun (signatureParts: NestedItemsArray<string>) ->        
        if (signatureParts.Length) - 1 <> (args |> Array.length) then GenExcError (sprintf "Expected %i arguments, got %i. Signature: %A" (signatureParts.Length - 1) (args |> Array.length) signatureParts ) else
        if args |> Array.map Syntax.nameIsInvalid |> Array.exists id then GenExcError "Invalid argument name." else

        parts[firstEqualI+1..] |> String.concat " " |> ToUnparsedManipulation
        |> map (fun fnBody ->
            {
                name = name
                unparsedSignature = signatureParts
                args = args
                fn = fnBody
                fnOptions = getFnOptions fnOptionStrings
            }
        )
    )

let rec separateFnOptions optionsAcc parts =
    if Array.contains parts[0] FunctionData.fnOptions then
        separateFnOptions (parts[0]::optionsAcc) parts.[1..]
    else
        optionsAcc, parts

let getFnOptions optionStrings =
    let isPostFix = List.contains "postfix" optionStrings
    let isInfix = List.contains "infix" optionStrings
    let isNoMemo = List.contains "noMemo" optionStrings

    {
        fixation = (if isPostFix || isInfix then (if isPostFix then Postfix else Infix) else Prefix)
        noMemo = isNoMemo
    }

let extractFullSignature rawSignature =
    let splitTypes (typeRef: string) =
        if not (typeRef.Contains '*') then Ok [| typeRef |] else 

            let typeRefSplit = typeRef.Split '*'

            if typeRefSplit.Length <> 2 then GenExcError ("Invalid type reference (asterisk found more than once): " + typeRef) else
            // checking if the type is a valid name is not necessary - they are references to defined types, which are checked (elsewhere)

            Array.init (int typeRefSplit[1]) (fun _ -> typeRefSplit[0]) |> Ok

    rawSignature
    |> String.concat " "
    |> Parsing.parseNestedByBrackets
    |> bind (fun x ->
        x
        |> Array.map (NestedItems.applyToInnerItems splitTypes)
        |> Array.map (NestedItems.combineResults)
        |> Result.combineToArray
        |> map NestedItems.concatLowestLevelItems
    )

// does not support precedence/nesting
let ToUnparsedManipulation line = line |> Parsing.parseNestedByBrackets

let ToUnparsedTypeDefinition line=
    let parts = line |> Parsing.trimSplit [| ' ' |]
    if parts.Length < 4 then GenExcError "Type definition missing parts. Missing space?" else

    let maybeFirstColon = parts |> Array.tryFindIndex (fun x -> x = ":")
    if maybeFirstColon.IsNone then GenExcError "Type definition missing a colon separated by spaces." else

    let name = parts[1]
    if Syntax.nameIsInvalid name then GenExcError ("Invalid type name: " + name) else

    extractFullSignature parts[maybeFirstColon.Value+1..]
    |> map (fun signatureParts -> { name = name; unparsedSignature = signatureParts })

let ParseLine loc definitionContext line  =
    match line with
    | UnparsedExpression m -> m |> ParseManipulation definitionContext |> map (fun manipulation -> Expression { manip = manipulation; loc = loc })
    | UnparsedAssignment f -> f |> ParseCallableFunction loc definitionContext |> map Assignment
    | UnparsedTypeDefinition t -> t |> ParseTypeDefinition loc definitionContext |> map TypeDefinition
    | UnparsedModuleDeclaration m -> parseModuleDeclaration loc m
    | UnparsedModuleReference m -> parseModuleReference loc m

let ParseCallableFunction loc definitionContext f =
    let maybeSignature = f.unparsedSignature |> NestedItems.toFnType definitionContext 
    let maybeFn = f.fn |> ParseManipulation {definitionContext with functions = Array.append definitionContext.functions f.args}

    joinTwoResults maybeSignature maybeFn
    |> bind (fun (signature, fn) ->
        {
            name = f.name
            signature = signature
            args = f.args
            manip = fn
            loc = loc
            fnOptions = f.fnOptions
        } |> Ok
    )

let ParseManipulation definitionContext m = m |> NestedItems.toManipulation definitionContext

let ParseTypeDefinition loc definitionContext t  =
    t.unparsedSignature 
    |> NestedItems.toFnType definitionContext
    |> map (fun signature -> { name = t.name; signature = signature; loc=loc })

let stringToType definitionContext s =
    match s with
    | s' when Array.contains s' definitionContext.types -> BaseFnType s' |> Ok |> toGenericResult
    | _ -> GenExcError ("Unknown type: " + s)

let parseModuleDeclaration loc line =
    match loc.fileLocation with
    | None -> GenExcError "Module declaration outside of file."
    | Some fileLoc -> 
        let dir = System.IO.Path.GetDirectoryName fileLoc
        
        // for now, module and files names must match
        if Files.toQualifiedFileLoc dir line <> fileLoc then GenExcError "Module declaration does not match file location." else

        line |> Files.toQualifiedFileLoc dir |> ModuleDeclaration |> Ok

let parseModuleReference loc line =
    let dir = getDirOfReference loc.fileLocation

    line
    |> Files.toQualifiedFileLoc dir 
    |> ModuleReference 
    |> Ok

let getDirOfReference fileLoc =
    match fileLoc with
    | Some fileLoc -> System.IO.Path.GetDirectoryName fileLoc
    | None -> Files.packageLocation

let toOrderedFile lines =
    // not used for now
    // let moduleDeclaration = lines |> Array.tryHead |> Option.bind (fun x -> match x with | ModuleDeclaration m -> Some m | _ -> None)
    let moduleReferences = lines |> Array.choose (fun x -> match x with | ModuleReference m -> Some m | _ -> None)
    let expressions = lines |> Array.choose (fun x -> match x with | Expression e -> Some e | _ -> None)
    let assignments = lines |> Array.choose (fun x -> match x with | Assignment a -> Some a | _ -> None)
    let typeDefinitions = lines |> Array.choose (fun x -> match x with | TypeDefinition t -> Some t | _ -> None)

    {
        // moduleDeclaration = moduleName
        moduleReferences = moduleReferences
        expressions = expressions
        assignments = assignments
        typeDefinitions = typeDefinitions
    }


module Parsing =
    let trimSplit cs s =
        s.Split cs
        |> Array.map (fun x -> x.Trim())
        |> Array.filter (fun x -> x.Length > 0)
        |> Array.collect (fun x ->
            if x.Length = 1 then [| x |] else

            if Array.contains x[0] (Syntax.specialChars.ToCharArray()) then [| x[0].ToString(); x.Substring(1) |] else
            if Array.contains x.[x.Length-1] (Syntax.specialChars.ToCharArray()) then [| x.Substring(0, x.Length-1); x[x.Length-1].ToString() |] else
            [| x |]
        )

    let trimSplitSimple (cs: char array) (s: string) =
        s.Split cs
        |> Array.map (fun x -> x.Trim())

    let trimSplitIndexedArray (cs: char array) (arr: (int * string) array) =
        arr
        |> Array.collect (fun (i, x) -> trimSplitSimple cs x |> Array.map (fun y -> i, y))

    let parseNestedByBrackets s = 
        let rec parseNestedByBracketsInner acc (splitS: string array) =
            if splitS.Length = 0 then 
                acc, splitS
            else if splitS[0] = ")"  then 
                acc, splitS[1..] // dont pass on the closing bracket
            else if splitS[0] = "(" then 
                let (innerAcc, rest) = parseNestedByBracketsInner [||] splitS[1..]
                parseNestedByBracketsInner (Array.append acc [|NestedArray innerAcc|]) rest
            else
                parseNestedByBracketsInner (Array.append acc [|NestedItem splitS[0]|]) splitS[1..]

        let maybeParanError = validateParantheses s
        if maybeParanError.IsSome then Error maybeParanError.Value else

        s 
        |> trimSplit [| ' ' |]
        |> parseNestedByBracketsInner [||]
        |> fst
        |> Ok

    let validateParantheses s : GenericException option =
        let mutable stack = Stack<char>()

        s
        |> Seq.fold (fun (maybeExc: GenericException option) c ->
            if maybeExc.IsSome then maybeExc else
            if stack.Count = 0 && c = ')' then Some(GenExc (sprintf "Too many closing parantheses: %c" c)) 
            else

                if c = '(' then stack.Push c
                else if c = ')' then stack.Pop() |> ignore

                None
        ) None
        |> Option.orElse (if stack.Count = 0 then None else Some(GenExc (sprintf "Unclosed parantheses: %A" stack)))

module NestedItems =

    let applyToItems f nestedItems  = nestedItems |> Array.map (fun x -> applyToInnerItems f x)

    let rec applyToInnerItems f nestedItems = 
        match nestedItems with 
        | NestedItem i -> f i |> NestedItem
        | NestedArray a -> a |> Array.map (applyToInnerItems f) |> NestedArray

    let rec combineResults nestedItems = 
        match nestedItems with
        // change this - it cant be right
        | NestedItem i ->
            match i with
            | Ok x -> x |> NestedItem |> Ok
            | Error e -> Error e
        | NestedArray a -> a |> Array.map combineResults |> Result.combineToArray |> map NestedArray

    let rec toFnType definitionContext nestedItems = 
        nestedItems
        |> Array.map (fun x -> 
            match x with 
            | NestedItem i -> i |> stringToType definitionContext
            | NestedArray a -> a |> toFnType definitionContext |> map Function
        )
        |> combineClacResultsToArray

    let rec toManipulation definitionCtx nestedItems = 
        nestedItems
        |> Array.map (fun x -> 
            match x with 
            | NestedItem reference ->
                if Array.contains reference definitionCtx.functions || Primitive.isPrim reference then
                    reference |> Fn |> Ok |> toGenericResult
                else
                    GenExcError ("Unknown function: " + reference)
            | NestedArray a -> a |> toManipulation definitionCtx |> map Manipulation
        )
        |> combineClacResultsToArray

    let concatLowestLevelItems nestedItems =
        let rec concatLowestLevelItemsInner nestedItems =
            nestedItems
            |> Array.collect (fun nestedItems ->
                match nestedItems with
                | NestedItem iArr -> iArr |> Array.map NestedItem
                | NestedArray innerArray ->
                    let processedInnerArray = innerArray |> concatLowestLevelItemsInner |> NestedArray
                    [| processedInnerArray |]
            )

        nestedItems 
        |> concatLowestLevelItemsInner
