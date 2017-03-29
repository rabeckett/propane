module TestGenerator

open CGraph
open Microsoft.Z3
open Topology
open Abgp   

type T

val genTest : CGraph.T -> Route.Predicate -> Set<Set<CgState*CgState>>
val generateRouterIp : Topology.T -> Topology.TopoInfo -> Map<string, string> 
val runTest : unit