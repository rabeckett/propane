module Input

open Common.Error
open System.IO
open Microsoft.FSharp.Text.Lexing

let setInitialPos (lexbuf: LexBuffer<_>) fname = 
    lexbuf.EndPos <- {pos_bol = 0; pos_fname = fname; pos_cnum = 0; pos_lnum = 1}

let readFromFile fname : Ast.T =
    let text = File.ReadAllText fname
    let lines = File.ReadLines fname |> Seq.toArray
    let lexbuf = LexBuffer<_>.FromString text
    setInitialPos lexbuf fname
    try 
        let defs, cs = Parser.start Lexer.tokenize lexbuf
        {Input=lines; Defs=defs; CConstraints=cs}
    with
        | Lexer.EofInComment ->
            parseError ("End of file detected in comment")
        | _ ->
            let pos = lexbuf.EndPos
            let line = pos.Line
            let column = pos.Column
            parseError (sprintf "Line: %d, Char: %d" line column)