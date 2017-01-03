module TestGenerator

open CGraph

type T

val genTest : CGraph.T -> T
val runTest : T -> unit