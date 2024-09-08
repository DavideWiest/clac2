module rec Clac2.Evaluator

open Clac2.Domain
open Clac2.Utilities

// watch out for primitive types


let evaluate (stdCtx: StandardContext) (lines: Line array) =
    Ok [| 0 |]
