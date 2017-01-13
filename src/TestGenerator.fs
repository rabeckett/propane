module TestGenerator

open Microsoft.Z3

type T = 
    {
        Model : Solver
    }

let genTest (input: CGraph.T) (ctx : Context) : Solver =
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in
    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBoolConst ("v" + (string i)));
    let eArray = Array.zeroCreate (Seq.length edges) in
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set eArray i (ctx.MkBoolConst ("e" + (string i)));
    ctx.MkSolver ()

let runTest =
    ()