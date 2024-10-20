module rec Clac2.Core.Exc.Domain

open Clac2.Core.Domain

type SimpleExc = {
    message: string
}

type SimpleResult<'a> = Result<'a, SimpleExc>

type IntermediateExc = {
    innerExc: SimpleExc
    line: int option
}

type IntermediateResult<'a> = Result<'a, IntermediateExc>

type FullExc = {
    innerExc: IntermediateExc
    fileLocation: string option
    maybeTrace: ProgramLocation list option
}

type FullResult<'a> = Result<'a, FullExc>