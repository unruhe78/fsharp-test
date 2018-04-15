// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "OperationResult.fs"
#load "Convert.fs"
#load "Parsers.fs"

open Commons.Types
open Commons.Parser
open Commons.Convert

"cake"
|> stringToCharList
|> run (expectString "coke")

