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

[<Struct>]
type Node =
    val Loc: string 
    val Typ: NodeType
    new: string * NodeType -> Node

type T = BidirectionalGraph<Node,Edge<Node>>


/// Make a defensive copy of the topology
val copyTopology: T -> T

/// Build the internal and external alphabet from a topology
val alphabet: T -> Set<Node> * Set<Node> 

/// Check if a node is a valid topology node
val isTopoNode: Node -> bool

/// Check if a node represents an external location (external AS)
val isOutside: Node -> bool

/// Check if a node represents an internal location (under AS control)
val isInside: Node -> bool

/// Check if a node can originate traffice (e.g., TOR in DC)
val canOriginateTraffic: Node -> bool

/// Checks if a topology is well-formed. This involves checking 
/// for duplicate names, as well as checking that the inside is fully connected
val isWellFormed: T -> bool

/// Helper function for building topology
val addVertices: T -> Node list -> unit 

/// Helper function for building topology
val addEdgesDirected: T -> (Node*Node) list -> unit

/// Helper function for building topology
val addEdgesUndirected: T -> (Node*Node) list -> unit

/// Find all the valid topology links corresponding to pairs of locations
val findLinks: T -> Set<string> * Set<string> -> (Node * Node) list

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
    type Tiers = Dictionary<Node,int>
    type Prefixes = Dictionary<Node,Prefix.T>
    val fatTree: int -> T * Prefixes * Tiers
    // Full mesh topology
    val complete: int -> T

/// Module with unit tests
module Test = 
    val run: unit -> unit