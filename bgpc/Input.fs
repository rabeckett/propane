module Input

open System.IO
open Microsoft.FSharp.Text.Lexing


let readFromString s =
    let lexbuf = LexBuffer<_>.FromString s
    Parser.start Lexer.tokenize lexbuf

let readFromFile f =
    readFromString (File.ReadAllText f)


