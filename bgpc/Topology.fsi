module Topology
open QuickGraph

type NodeType = 
    | Start
    | End
    | Outside
    | Inside 
    | InsideOriginates

type State = 
    {Loc: string; 
     Typ: NodeType}

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

val alphabet: T -> Set<State> * Set<State> 
val isTopoNode: State -> bool
val isInside: State -> bool
val canOriginateTraffic: State -> bool
val isWellFormed: State -> bool

module Failure = 
    type FailType = 
        | NodeFailure of State 
        | LinkFailure of TaggedEdge<State,unit>
    val allFailures: int -> T -> seq<FailType list>

module Example1 = 
    val topo: unit -> T

module Example2 = 
    val topo: unit -> T

module Example3 = 
    val topo: unit -> T