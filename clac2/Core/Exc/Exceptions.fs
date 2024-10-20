module rec Clac2.Core.Exc.Exceptions

open Clac2.Core.Exc.Domain
open Clac2.Core.Utils
open FSharp.Core.Result

// GenericException and ClacResult

let SimpleExc (e: string) = { message = e }
let SimpleResult (e: string) = e |> SimpleExc |> Error

let toGenericResult (result: Result<'a, string>) : SimpleResult<'a> =
    match result with
    | Ok v -> Ok v
    | Error e -> SimpleResult e

// IntermediateGenericException and IntermediateClacResult

let IntermediateExc (line: int) (e: SimpleExc) = { innerExc = e; line = Some line }
let IntermediateExcWithoutLine (e: SimpleExc) = { innerExc = e; line = None}
let IntermediateExcMaybeLine maybeLine (e: SimpleExc) = { innerExc = e; line = maybeLine }
let IntermediateExcFromParts (e: string) (line: int) = e |> SimpleExc |> IntermediateExc line
let IntermediateExcFPPure (e: string) (line: int) = e |> SimpleExc |> IntermediateExc line
let IntermediateExcFPPureMaybeLine (e: string) maybeLine = e |> SimpleExc |> IntermediateExcMaybeLine maybeLine
let toIntermediateResult (line: int) (result: SimpleResult<'a>) : IntermediateResult<'a> = result |> mapError (fun e -> IntermediateExc line e)
let toIntermediateResultWithoutLine (result: SimpleResult<'a>) : IntermediateResult<'a> = result |> mapError IntermediateExcWithoutLine
let tupledToIntermediateResult (result: int * SimpleResult<'a>) : IntermediateResult<'a> = result |> Tuple.applyUnpacked toIntermediateResult 

// FullGenericException and FullClacResult

let FullExc (location: string option) (genExcWithLine: IntermediateExc) = { innerExc = genExcWithLine; fileLocation = location; maybeTrace = None }
let FullExcFromParts (e: string) (line: int) (location: string option) = e |> SimpleExc |> IntermediateExc line |> FullExc location |> Error

let toFullResult (location: string option) (result: IntermediateResult<'a>) : FullResult<'a> = result |> mapError (fun e -> FullExc location e)
let tupledToFullExc (result: string option * IntermediateResult<'a>) : FullResult<'a> = result |> Tuple.applyUnpacked toFullResult

let addLocTraceToExc locTrace (e: FullExc) = { e with maybeTrace = Some locTrace }
let addLocTraceToResult locTrace (r: FullResult<'a>) = r |> mapError (addLocTraceToExc locTrace)
