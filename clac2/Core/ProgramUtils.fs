module rec Clac2.Core.ProgramUtils

let resultToReturnCode x =
    match x with 
    | Ok _ -> 0
    | Error _ -> 1