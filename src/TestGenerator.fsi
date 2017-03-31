module TestGenerator

open CGraph
open Microsoft.Z3
open Topology  

type T

val genTest : CGraph.T -> Route.Predicate -> Set<Set<CgState*CgState>>
val generateRouterIp : Topology.T -> Map<string, string> 
val runTest : unit
val writeTopoCBGP : Topology.T -> string -> unit