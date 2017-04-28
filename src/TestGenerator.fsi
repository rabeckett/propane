module TestGenerator

open CGraph
open Microsoft.Z3
open Topology  

type T

val genLinkTest : CGraph.T -> int -> Route.Predicate -> Set<Set<CgState*CgState>*Set<CgState*CgState>>*float
val genPrefTest : CGraph.T -> int -> Route.Predicate -> Set<Set<CgState*CgState>*Set<CgState*CgState>>*float
val generateRouterIp : Topology.T -> Map<string, string> 
val runTest : unit
val writeTopoCBGP : Topology.T -> string -> unit
val geteBGPStaticRoutes : Map<Topology.Node, Set<Topology.Node>> -> Map<string, string> -> string -> unit