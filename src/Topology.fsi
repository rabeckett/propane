module Topology

open QuickGraph
open System.Collections.Generic

type NodeType = 
  | Start
  | End
  | Outside
  | Inside
  | Unknown

[<Struct>]
type Node = 
  val Loc : string
  val Typ : NodeType
  new : string * NodeType -> Node

type T

/// Make a defensive copy of the topology
val copyTopology : T -> T
/// Build the internal and external alphabet from a topology
val alphabet : T -> Set<Node> * Set<Node>
//  Return the nodes in the topology
val vertices : T -> seq<Node>
//  Return all the edges in the topology
val edges : T -> seq<Node * Node>
//  Return all incoming edges for a given node
val inEdges : T -> Node -> seq<Node * Node>
//  Return all outgoing edges for a given node
val outEdges : T -> Node -> seq<Node * Node>
//  Return all the topological neighbors of a node
val neighbors : T -> Node -> seq<Node>
/// Check if a node is a valid topology node
val isTopoNode : Node -> bool
/// Check if a node represents an external location (external AS)
val isOutside : Node -> bool
/// Check if a node represents an internal location (under AS control)
val isInside : Node -> bool
/// Check if a node can originate traffice (e.g., TOR in DC)
val canOriginateTraffic : Node -> bool
/// Checks if a topology is well-formed. This involves checking 
/// for duplicate names, as well as checking that the inside is fully connected
val isWellFormed : T -> bool
/// Helper function for building topology
val addVertices : T -> Node list -> unit
/// Helper function for building topology
val addEdgesDirected : T -> (Node * Node) list -> unit
/// Helper function for building topology
val addEdgesUndirected : T -> (Node * Node) list -> unit
/// Find all the valid topology links corresponding to pairs of locations
val findLinks : T -> Set<string> * Set<string> -> (Node * Node) list
/// Find a topology node by location name
val findByLoc : T -> string -> Node option
/// Find peer topology nodes
val peers : T -> Node -> seq<Node>

type Constraint = 
  { Name : string
    Formula : string }

type TopoInfo = 
  { Graph : T
    NetworkAsn : int
    AsnMap : Map<string, int>
    InternalNames : Set<string>
    ExternalNames : Set<string>
    AllNames : Set<string>
    IpMap : Dictionary<string * string, string * string>
    NodeConstraints : Map<string, Constraint>
    EdgeConstraints : Map<string * string, Constraint * Constraint> }

val router : string -> TopoInfo -> string
/// Read a topology from an XML file
val readTopology : string -> TopoInfo

/// Examples of useful topologies for testing
module Examples = 
  type Tiers = Dictionary<Node, int>
  
  type Prefixes = Dictionary<Node, Route.Prefix>
  
  val topoDisconnected : unit -> T
  val topoDiamond : unit -> T
  val topoDatacenterSmall : unit -> T
  val topoDatacenterMedium : unit -> T
  val topoDatacenterMediumAggregation : unit -> T
  val topoDatacenterLarge : unit -> T
  val topoBadGadget : unit -> T
  val topoBrokenTriangle : unit -> T
  val topoBigDipper : unit -> T
  val topoSeesaw : unit -> T
  val topoStretchingManWAN : unit -> T
  val topoStretchingManWAN2 : unit -> T
  val topoPinCushionWAN : unit -> T
  val topoBackboneWAN : unit -> T
  val fatTree : int -> T * Prefixes * Tiers
  val complete : int -> T

/// Module with unit tests
module Test = 
  val run : unit -> unit