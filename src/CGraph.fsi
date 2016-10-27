module CGraph

open QuickGraph
open System.Collections
open System.Collections.Generic
open Util.Error

/// Individual node in the Product Graph. Contains:
///
/// .Id     a unique id to speed up hashing/comparisons in graph algorithms
/// .State  a unique representation of all automata states (q1,...,qk)
/// .Accept a 16 bit integer representing the lowest (best) preference
///         the value System.Int16.MaxValue represents a non-accepting node
/// .Node   the underlying topology node, including name + internal/external
[<CustomEquality; CustomComparison>]
type CgState = 
   { Id : int
     State : int
     Accept : int16
     Node : Topology.Node }
   interface System.IComparable

/// Type of the Product Graph. Contains:
/// 
/// .Start  A unique start node. Anything routers connected 
///         to the start node can originate traffic
/// .End    A unique end node, which is connected to all accepting
///         nodes. This is included to simplify some algorithms
/// .Topo   The underlying topology object
type T = 
   { Start : CgState
     End : CgState
     Graph : BidirectionalGraph<CgState, Edge<CgState>>
     Topo : Topology.T }

/// Direction of search. We often need to search in the reverse graph,
/// yet do not want to make a copy of the graph every time
type Direction = 
   | Up
   | Down

/// Make a shallow-ish copy of the graph. Does not clone node values.
val copyGraph : T -> T
/// Make a shallow-ish copy of the graph, and reverses all edges
val copyReverseGraph : T -> T
/// Constructs a new, product automaton from the topology and a collection 
/// of DFAs for path expressions
val buildFromAutomata : Topology.T -> Regex.Automaton array -> T
/// Get the location for the state
val loc : CgState -> string
/// Determine if two nodes shadow each other
val shadows : CgState -> CgState -> bool
/// Returns the set of reachable preferences 
val preferences : T -> Set<int16>
/// Returns the set of states that are attached to the end node
val acceptingStates : T -> Set<CgState>
/// Returns the set of locations that are attached to the end node
val acceptingLocations : T -> Set<string>
/// Returns the (outgoing) neighbors of a state in the graph
val neighbors : T -> CgState -> seq<CgState>
/// Returns the (incoming) neighbors of a state in the graph
val neighborsIn : T -> CgState -> seq<CgState>
/// Return true when a node represents a repeated external location
val repeatedOuts : T -> Set<CgState>
/// Returns true if a node is not the special start or end node
val isRealNode : CgState -> bool
/// Returns true if a node is internal to the AS
val isInside : CgState -> bool
/// Returns true if the graph contains only the start and end nodes
val isEmpty : T -> bool
/// Convert the constraint graph to the DOT format for visualization
val toDot : T -> Ast.PolInfo -> string
/// Generate a png file for the constraint graph (requires graphviz dot utility)
val generatePNG : T -> Ast.PolInfo -> string -> unit

module Reachable = 
   /// Find all destinations reachable from src
   val dfs : T -> CgState -> Direction -> HashSet<CgState>
   /// Find all accepting preferences reachable from a given src
   val srcAccepting : T -> CgState -> Direction -> Set<int16>

module Minimize = 
   /// Remove nodes and edges not relevant to the BGP decision process
   val minimize : int -> Topology.TopoInfo -> T -> T

module Consistency = 
   type Neighbor = string
   
   type FailurePoint = CgState * CgState * CgState list
   
   type Explanation = (Neighbor * Neighbor) option * (FailurePoint * FailurePoint) option
   
   /// An explanation for why a policy is unimplementable with BGP
   type CounterExample = CgState * CgState * Explanation
   
   /// Preference ranking for each router based on possible routes
   type Preferences = seq<CgState>
   
   /// Preferences for each internal router
   type Ordering = Dictionary<string, Preferences>
   
   /// Conservative check if the BGP routers can make local decisions not knowing about failures
   /// Takes an optional file name for debugging intermediate information
   val findOrderingConservative : (int -> T -> Result<Ordering, CounterExample>)

module ToRegex = 
   /// Construct a compact regular expression describing the paths
   /// from a given node in the graph
   val constructRegex : T -> CgState -> Regex.T

module Failure = 
   /// A single node or link falure
   type FailType = 
      | NodeFailure of Topology.Node
      | LinkFailure of Topology.Node * Topology.Node
   
   /// Enumerate all failures up to a given size
   val allFailures : int -> Topology.T -> seq<FailType list>
   /// Create the corresponding failed product graph
   val failedGraph : T -> FailType list -> T
   /// Find the minimal number of failures to disconnect from an aggregate
   val disconnectLocs : T -> seq<CgState> -> string -> (int * string * string) option