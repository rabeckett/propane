module TestGenerator

open Z3

type T = 
    {
        Model : Z3.Solver
    }

let genTest (input: CGraph.T) (ctx : Z3.Context) : T =
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in
    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBookConst ("v" ^ (string_of_int i)));
    let eArray = Array.zeroCreate (Seq.length edges) in
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set eArray i (ctx.MkBookConst ("e" ^ (string_of_int i)));
    ctx.MkSolver ()

let runTest =
    ()