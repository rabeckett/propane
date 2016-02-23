module CGraph

open QuickGraph
open Common.Error
open System.Collections.Generic


[<CustomEquality; CustomComparison>]
type CgState = 
    {Id: int;
     State: int; 
     Accept: Set<int>; 
     Node: Topology.State;
     mutable Mark: uint32}

     interface System.IComparable

type T = 
    {Start: CgState;
     End: CgState;
     Graph: BidirectionalGraph<CgState, Edge<CgState>>;
     Topo: Topology.T;
     mutable Mark: uint32;}

/// Direction of search. We often need to search in the reverse graph,
/// yet do not want to make a copy of the graph every time
type Direction = Up | Down

/// Make a shallow copy of the graph. Does not clone node values.
val copyGraph: T -> T

/// Make a shallow copy of the graph, and reverses all edges
val copyReverseGraph: T -> T

/// Constructs a new, product automaton from the topology and a collection 
/// of DFAs for path expressions
val buildFromAutomata: Topology.T -> Regex.Automaton array -> T

/// Get the location for the state
val inline loc: CgState -> string

/// Determine if two nodes shadow each other
val inline shadows: CgState -> CgState -> bool

/// Returns the set of reachable preferences
val inline preferences: T -> Set<int>

/// Returns the set of states that are attached to the end node
val inline acceptingStates: T -> Set<CgState>

/// Returns the set of locations that are attached to the end node
val inline acceptingLocations: T -> Set<string>

/// Returns the (outgoing) neighbors of a state in the graph
val inline neighbors: T -> CgState -> seq<CgState>

/// Returns the (incoming) neighbors of a state in the graph
val inline neighborsIn: T -> CgState -> seq<CgState> 

/// Return true when a node represents a repeated external location
val inline isRepeatedOut: T -> CgState -> bool

/// Returns true if a node is not the special start or end node
val inline isRealNode: CgState -> bool

/// Returns true if the graph contains only the start and end nodes
val inline isEmpty: T -> bool 

/// Returns a copy of the graph, restricted to nodes for a given preference
val restrict: T -> int -> T

/// Convert the constraint graph to the DOT format for visualization
val toDot: T -> string

/// Generate a png file for the constraint graph (requires graphviz dot utility)
val generatePNG: T -> string -> unit


module Reachable =
    /// Find all destinations reachable from src while avoiding certain nodes
    val srcWithout: T -> CgState -> (CgState -> bool) -> Direction -> HashSet<CgState>

    /// Check if src can reach dst while avoiding certain nodes
    val inline srcDstWithout: T -> CgState -> CgState -> (CgState -> bool) -> Direction -> bool

    /// Check if src can reach dst
    //val inline srcDst: T -> CgState -> CgState -> Direction -> bool

    /// Find all destinations reachable from src
    val inline src: T -> CgState -> Direction -> HashSet<CgState>

    /// Final all reachable preference levels
    val inline srcAccepting: T -> CgState -> Direction -> Set<int>


module Minimize =
    /// Get rid of nodes that can originate traffic but aren't accepting
    val delMissingSuffixPaths: T -> T

    /// Fixpoint removal of nodes and edges, including nodes not on any simple path
    val minimize: int -> T -> T


module Consistency =
    /// An explanation for why a policy is unimplementable with BGP
    type CounterExample = CgState * CgState

    /// Preference ranking for each router based on possible routes
    type Preferences = seq<CgState>

    /// Preferences for each internal router
    type Ordering = Map<string, Preferences>

    /// Conservative check if the BGP routers can make local decisions not knowing about failures
    /// Takes an optional file name for debugging intermediate information
    val findOrderingConservative: (int -> T -> string -> Result<Ordering, CounterExample>)


module ToRegex = 

    /// Construct a compact regular expression describing the paths
    /// from a given node in the graph
    val constructRegex: T -> CgState -> Regex.T


module Failure =
    /// A single node or link falure
    type FailType =
        | NodeFailure of Topology.State
        | LinkFailure of Edge<Topology.State>

    /// Enumerate all failures up to a given size
    val allFailures: int -> Topology.T -> seq<FailType list>

    /// Create the corresponding failed product graph
    val failedGraph: T -> FailType list -> T

    /// Find the minimal number of failures to disconnect from an aggregate
    val disconnectLocs: T -> seq<CgState> -> string -> (int * string * string) option