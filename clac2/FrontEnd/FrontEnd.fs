module rec Clac2.FrontEnd.FrontEnd

open FSharp.Core.Result
open Clac2.Core.Utils
open Clac2.Core.Domain
open Clac2.Core.Context
open Clac2.Core.Exc.Exceptions
open Clac2.Core.Lang.Language
open Clac2.FrontEnd.Util
open Clac2.FrontEnd.Domain
open Clac2.FrontEnd.NestedItems
open Clac2.FrontEnd.Parsing

let preparse input =
    input
    |> Parsing.trimSplitSimple [| '\n' |]
    |> Array.mapi (fun i x -> (i, x))
    |> Array.map (fun (i, x) -> i, String.cutOffAt x Syntax.commentIdentifer)
    |> Array.filter (fun x -> not ((snd x).StartsWith Syntax.commentIdentifer))
    |> Parsing.trimSplitIndexedArray [| ';' |]
    |> Array.filter (fun x -> (snd x) <> "")
    |> Array.map (Tuple.applyUnpacked ToUnparsedLine)
    |> Result.combineToArray

let parseFullResult fileLoc defCtx preParsedLines = parse fileLoc defCtx preParsedLines |> Full.toResult fileLoc 

let parse fileLoc (defCtx: DefinitionContext) preParsedLines =  
    let (localTypesWithLines, localFunctionsWithLines) = extractCustomDefinitionsWithLine preParsedLines

    let duplicationError arr source symbolName = 
        let maybeDup = Array.chooseHigherOccurenceElements 1 arr source |> Array.tryHead
        match maybeDup with
        | Some (dupLine, dupName) -> Intermediate.ExcFromParts ("Duplicate " + symbolName + " definition for: " + dupName) dupLine |> Some
        | None -> None

    let dupFnResult = duplicationError localFunctionsWithLines defCtx.functions "function"
    if dupFnResult.IsSome then Error dupFnResult.Value else
    let dupTypeResult = duplicationError localTypesWithLines defCtx.types "type"
    if dupTypeResult.IsSome then Error dupTypeResult.Value else

    preParsedLines
    |> Array.map (fun (i, l) -> i, ParseLine (buildLoc fileLoc i) defCtx l)
    |> Array.map (fun (i, r) -> Intermediate.toResult i r)
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
    line |> ToUnparsedLineInner |> Intermediate.toResult i |> map (fun x -> (i, x))

let ToUnparsedLineInner line =
    if line.StartsWith "module " then
        line.Substring(7) |> UnparsedModuleDeclaration |> Ok
    else if line.StartsWith "open " then
        line.Substring(5) |> UnparsedModuleReference |> Ok
    else if line.StartsWith "let " then
        line |> ToUnparsedCallableFunction |> map UnparsedAssignment
    else if line.StartsWith "type " then
        line |> ToUnparsedTypeDefinition |> map UnparsedTypeDefinition
    else
        line |> ToUnparsedManipulation |> map UnparsedExpression

let ToUnparsedCallableFunction line =
    let partsBefore: string array = line |> Parsing.trimSplit [| ' ' |]

    if partsBefore.Length < 2 then Simple.toExcResult "Assignment missing parts. Missing space?" else
    
    let fnOptionStrings, (parts: string array) = separateFnOptions [] partsBefore[1..]

    if List.contains "infix" fnOptionStrings && List.contains "postfix" fnOptionStrings then Simple.toExcResult "Function cannot be both infix and postfix." else 
    if parts.Length < 5 then Simple.toExcResult "Assignment missing parts. Missing space?" else

    let firstColonI = parts |> Array.findIndex (fun x -> x = ":")
    let firstEqualI = parts |> Array.findIndex (fun x -> x = "=")

    if firstColonI = -1 || firstEqualI = -1 then Simple.toExcResult "Assignment missing a colon or an equals." else
    if firstColonI > firstEqualI then Simple.toExcResult "Assignment is missing a colon before the assignment." else
    if firstColonI = firstEqualI - 1 then Simple.toExcResult "Assignment missing type signature between colon and equals." else
    if parts.Length = firstEqualI + 1 then Simple.toExcResult "Assignment missing function body." else

    let nameAndArgs = parts[..firstColonI-1]
    let name , args = nameAndArgs[0], nameAndArgs[1..]
    if Syntax.nameIsInvalid name then Simple.toExcResult ("Invalid function name: " + name) else

    extractFullSignature parts[firstColonI+1..firstEqualI-1]
    |> bind (fun (signatureParts: NestedItemArray<string>) ->        
        if (signatureParts.Length) - 1 <> (args |> Array.length) then Simple.toExcResult (sprintf "Expected %i arguments, got %i. Signature: %A" (signatureParts.Length - 1) (args |> Array.length) signatureParts ) else
        if args |> Array.map Syntax.nameIsInvalid |> Array.exists id then Simple.toExcResult "Invalid argument name." else

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
    if Array.contains parts[0] FuncData.fnOptions then
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

            if typeRefSplit.Length <> 2 then Simple.toExcResult ("Invalid type reference (asterisk found more than once): " + typeRef) else
            // checking if the type is a valid name is not necessary - they are references to defined types, which are checked (elsewhere)

            Array.init (int typeRefSplit[1]) (fun _ -> typeRefSplit[0]) |> Ok

    rawSignature
    |> String.concat " "
    |> Parsing.parseNestedByBrackets
    |> bind (fun x ->
        x
        |> Array.map (NestedItems.applyToInnerItems splitTypes)
        |> Array.map (NestedItems.combineNestedItemResults)
        |> Result.combineToArray
        |> map NestedItems.concatLowestLevelItems
    )

// does not support precedence/nesting
let ToUnparsedManipulation line = line |> Parsing.parseNestedByBrackets

let ToUnparsedTypeDefinition line =
    let parts = line |> trimSplit [| ' ' |]
    if parts.Length < 4 then Simple.toExcResult "Type definition missing parts. Missing space?" else

    let maybeFirstColon = parts |> Array.tryFindIndex (fun x -> x = ":")
    if maybeFirstColon.IsNone then Simple.toExcResult "Type definition missing a colon separated by spaces." else

    let name = parts[1]
    if Syntax.nameIsInvalid name then Simple.toExcResult ("Invalid type name: " + name) else

    extractFullSignature parts[maybeFirstColon.Value+1..]
    |> map (fun signatureParts -> { name = name; unparsedSignature = signatureParts })

let ParseLine loc (definitionContext: DefinitionContext) line  =
    match line with
    | UnparsedExpression m -> m |> ParseManipulation definitionContext |> map (fun manipulation -> Expression { manip = manipulation; loc = loc })
    | UnparsedAssignment f -> f |> ParseCallableFunction loc definitionContext |> map Assignment
    | UnparsedTypeDefinition t -> t |> ParseTypeDefinition loc definitionContext |> map TypeDefinition
    | UnparsedModuleDeclaration m -> parseModuleDeclaration loc m
    | UnparsedModuleReference m -> parseModuleReference loc m

let ParseCallableFunction loc definitionContext f =
    let maybeSignature = f.unparsedSignature |> NestedItems.toFnType definitionContext 
    let maybeFn = f.fn |> ParseManipulation {definitionContext with functions = Array.append definitionContext.functions f.args}

    Result.combineTwo maybeSignature maybeFn
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

let ParseManipulation (definitionContext: DefinitionContext) m = m |> NestedItems.toManipulation definitionContext

let ParseTypeDefinition loc definitionContext t  =
    t.unparsedSignature 
    |> NestedItems.toFnType definitionContext
    |> map (fun signature -> { name = t.name; signature = signature; loc=loc })


let parseModuleDeclaration loc line =
    match loc.fileLocation with
    | None -> Simple.toExcResult "Module declaration outside of file."
    | Some fileLoc -> 
        let dir = System.IO.Path.GetDirectoryName fileLoc
        
        // for now, module and files names must match
        if Files.toQualifiedFileLoc dir line <> fileLoc then Simple.toExcResult "Module declaration does not match file location." else

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