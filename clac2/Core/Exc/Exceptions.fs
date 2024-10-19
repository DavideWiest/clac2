module rec Clac2.Core.Exc.Exceptions

open Clac2.Core.Exc.Domain
open Clac2.Core.Utils
open FSharp.Core.Result

// GenericException and ClacResult

let GenExc (e: string) = { message = e }
let GenExcError (e: string) = e |> GenExc |> Error

let toGenericResult (result: Result<'a, string>) : GenericResult<'a> =
    match result with
    | Ok v -> Ok v
    | Error e -> GenExcError e

// IntermediateGenericException and IntermediateClacResult

let IntermediateExc (line: int) (e: GenericException) = { genExc = e; line = Some line }
let IntermediateExcWithoutLine (e: GenericException) = { genExc = e; line = None}
let IntermediateExcMaybeLine maybeLine (e: GenericException) = { genExc = e; line = maybeLine }
let IntermediateExcFromParts (e: string) (line: int) = e |> GenExc |> IntermediateExc line
let IntermediateExcFPPure (e: string) (line: int) = e |> GenExc |> IntermediateExc line
let IntermediateExcFPPureMaybeLine (e: string) maybeLine = e |> GenExc |> IntermediateExcMaybeLine maybeLine
let toIntermediateResult (line: int) (result: GenericResult<'a>) : IntermediateClacResult<'a> = result |> mapError (fun e -> IntermediateExc line e)
let toIntermediateResultWithoutLine (result: GenericResult<'a>) : IntermediateClacResult<'a> = result |> mapError IntermediateExcWithoutLine
let tupledToIntermediateResult (result: int * GenericResult<'a>) : IntermediateClacResult<'a> = result |> applyUnpacked toIntermediateResult 

// FullGenericException and FullClacResult

let FullExc (location: string option) (genExcWithLine: IntermediateException) = { genExcWithLine = genExcWithLine; fileLocation = location; locTrace = None }
let FullExcFromParts (e: string) (line: int) (location: string option) = e |> GenExc |> IntermediateExc line |> FullExc location |> Error

let toFullResult (location: string option) (result: IntermediateClacResult<'a>) : FullClacResult<'a> = result |> mapError (fun e -> FullExc location e)
let tupledToFullExc (result: string option * IntermediateClacResult<'a>) : FullClacResult<'a> = result |> applyUnpacked toFullResult

let addLocTraceToExc locTrace (e: FullGenericException) = { e with locTrace = Some locTrace }
let addLocTraceToResult locTrace (r: FullClacResult<'a>) = r |> mapError (addLocTraceToExc locTrace)
