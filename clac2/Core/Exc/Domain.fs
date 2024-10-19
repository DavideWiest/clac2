module rec Clac2.Core.Exc.Domain

open Clac2.Core.Domain

type GenericException = {
    message: string
}

type GenericResult<'a> = Result<'a, GenericException>

type IntermediateException = {
    genExc: GenericException
    line: int option
}

type IntermediateClacResult<'a> = Result<'a, IntermediateException>

type FullGenericException = {
    genExcWithLine: IntermediateException
    fileLocation: string option
    locTrace: ProgramLocation list option
}

type FullClacResult<'a> = Result<'a, FullGenericException>