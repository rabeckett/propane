module TestGenerator

open CGraph
open Microsoft.Z3

type T

val genTest : CGraph.T -> Solver
val runTest : T -> unit