module Input

open Microsoft.FSharp.Text.Lexing
open System.IO
open Util.Error
open Util.Format

let setInitialPos (lexbuf : LexBuffer<_>) fname = 
  lexbuf.EndPos <- { pos_bol = 0
                     pos_fname = fname
                     pos_cnum = 0
                     pos_lnum = 1 }

let readFromFile fname = 
  let text = File.ReadAllText fname
  let lines = File.ReadLines fname |> Seq.toArray
  let lexbuf = LexBuffer<_>.FromString text
  setInitialPos lexbuf fname
  try 
    let defs, cs = Parser.start Lexer.tokenize lexbuf
    (lines, defs, cs)
  with
    | Lexer.EofInComment -> 
      writeColor "Error: " System.ConsoleColor.DarkRed
      printfn "End of file detected in comment"
      exit 0
    | e -> 
      let pos = lexbuf.EndPos
      let line = pos.Line
      let column = pos.Column
      let settings = Args.getSettings()
      writeColor "Error:" System.ConsoleColor.DarkRed
      printfn " Line %d, Column %d" line column
      exit 0