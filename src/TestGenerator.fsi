module TestGenerator

open CGraph
open Microsoft.Z3
open Topology

type T

val genTest : CGraph.T -> unit
val runTest : unit