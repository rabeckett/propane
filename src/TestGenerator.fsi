module TestGenerator

open CGraph
open Microsoft.Z3
open Topology
open Abgp   

type T

val genTest : T -> CGraph.T -> Route.Predicate -> unit
val newTest : unit -> T
val runTest : unit