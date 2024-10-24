module rec Clac2.Core.Exc.Exceptions

open Clac2.Core.Exc.Domain
open Clac2.Core.Utils
open FSharp.Core.Result

module Simple =
    let Exc (e: string) = { message = e }
    let toExcResult (e: string) = e |> Exc |> Error

    let toResult r = r |> mapError Exc

module Intermediate =

    let Exc line e = { innerExc = e; line = Some line }
    let ExcWithoutLine e = { innerExc = e; line = None}
    let ExcMaybeLine maybeLine e = { innerExc = e; line = maybeLine }
    let ExcFromParts e line = e |> Simple.Exc |> Exc line
    let ExcFPPure e line = e |> Simple.Exc |> Exc line
    let ExcFPPureMaybeLine e maybeLine = e |> Simple.Exc |> ExcMaybeLine maybeLine

    let toResult line result = result |> mapError (Exc line)
    let toResultWithoutLine result  = result |> mapError ExcWithoutLine
    let tupledToResult result = result |> Tuple.applyUnpacked toResult 

module Full =

    let Exc location innerExc = { innerExc = innerExc; fileLocation = location; maybeTrace = None }

    let toResult location result = result |> mapError (fun e -> Exc location e)
    let tupledToExc result = result |> Tuple.applyUnpacked toResult

    let addLocTraceToExc locTrace e = { e with maybeTrace = Some locTrace }
    let addLocTraceToResult locTrace r = r |> mapError (addLocTraceToExc locTrace)

module ErrPipe = 
    let toFullExcFromParts line maybeFilePath e = e |> Simple.Exc |> Intermediate.ExcMaybeLine line |> Full.Exc maybeFilePath
