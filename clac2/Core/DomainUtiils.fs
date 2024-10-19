module rec Clac2.Core.DomainUtils

open Clac2.Core.Domain

let buildLoc fileLoc lineLoc = { fileLocation = fileLoc; lineLocation = lineLoc }
