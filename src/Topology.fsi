module Topology

open QuickGraph
open System.Collections.Generic

type NodeType = 
    | Start
    | End
    | Outside
    | Inside 
    | InsideOriginates
    | Unknown

type State = 
    {Loc: string; 
     Typ: NodeType}

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>


/// Make a defensive copy of the topology
val copyTopology: T -> T

/// Build the internal and external alphabet from a topology
val alphabet: T -> Set<State> * Set<State> 

/// Check if a node is a valid topology node
val isTopoNode: State -> bool

/// Check if a node represents an external location (external AS)
val isOutside: State -> bool

/// Check if a node represents an internal location (under AS control)
val isInside: State -> bool

/// Check if a node can originate traffice (e.g., TOR in DC)
val canOriginateTraffic: State -> bool

/// Checks if a topology is well-formed. This involves checking 
/// for duplicate names, as well as checking that the inside is fully connected
val isWellFormed: T -> bool

/// Helper function for building topology
val addVertices: T -> State list -> unit 

/// Helper function for building topology
val addEdgesDirected: T -> (State*State) list -> unit

/// Helper function for building topology
val addEdgesUndirected: T -> (State*State) list -> unit

/// Find all the valid topology links corresponding to pairs of locations
val findLinks: T -> Set<string> * Set<string> -> (State * State) list

type TopoInfo =
    {Graph: T; 
     AsnMap: Map<string, uint32>;
     InternalNames: Set<string>;
     ExternalNames: Set<string>;
     AllNames: Set<string>}

val router: string -> TopoInfo -> string

/// Read a topology from an XML file
val readTopology: string -> TopoInfo

/// Examples of useful topologies for testing
module Examples = 
    val topoDisconnected: unit -> T
    val topoDiamond: unit -> T
    val topoDatacenterSmall: unit -> T 
    val topoDatacenterMedium: unit -> T
    val topoDatacenterMediumAggregation: unit -> T
    val topoDatacenterLarge: unit -> T
    val topoBadGadget: unit -> T
    val topoBrokenTriangle: unit -> T
    val topoBigDipper: unit -> T
    val topoSeesaw: unit -> T
    val topoStretchingManWAN: unit -> T
    val topoStretchingManWAN2: unit -> T
    val topoPinCushionWAN: unit -> T
    val topoBackboneWAN: unit -> T
    /// Fattree topology 
    type Tiers = Dictionary<State,int>
    type Prefixes = Dictionary<State,Prefix.T>
    val fatTree: int -> T * Prefixes * Tiers
    // Full mesh topology
    val complete: int -> T

/// Module with unit tests
module Test = 
    val run: unit -> unit