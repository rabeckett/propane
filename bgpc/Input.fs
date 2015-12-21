module Input

open Common.Error
open System.IO
open Microsoft.FSharp.Text.Lexing

let setInitialPos (lexbuf: LexBuffer<_>) fname = 
    lexbuf.EndPos <- {pos_bol = 0; pos_fname = fname; pos_cnum = 0; pos_lnum = 1}

let readFromFile fname =
    let text = File.ReadAllText fname
    let lexbuf = LexBuffer<_>.FromString text
    setInitialPos lexbuf fname
    try Parser.start Lexer.tokenize lexbuf
    with
        | Lexer.EofInComment ->
            parseError ("End of file detected in comment")
        | _ ->
           let pos = lexbuf.EndPos
           let line = pos.Line
           let column = pos.Column
           parseError (sprintf "Line: %d, Char: %d" line column)


