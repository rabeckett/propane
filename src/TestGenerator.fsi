module TestGenerator

open CGraph
open Microsoft.Z3
open Topology  

type T

val genLinkTest : CGraph.T -> Route.Predicate -> Set<Set<CgState*CgState>>
val genPrefTest : CGraph.T -> Route.Predicate -> Set<Set<CgState*CgState>>
val generateRouterIp : Topology.T -> Map<string, string> 
val runTest : unit
val writeTopoCBGP : Topology.T -> string -> unit
val geteBGPStaticRoutes : Map<CgState, Set<CgState>> -> Map<string, string> -> string -> unit