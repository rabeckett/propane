module CGraph
open QuickGraph
open Common.Error

type CgState = 
    {States: int array; 
     Accept: Set<int>; 
     Node: Topology.State}

type T = 
    {Start: CgState;
     End: CgState;
     Graph: BidirectionalGraph<CgState, TaggedEdge<CgState, unit>>;
     Topo: Topology.T}

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

/// Constructs a new, product automaton from the topology and a 
/// collection of regular expression automata for different route preferences.
/// All paths through the product graph are valid topology paths that satisfy
/// one or more of the regular path constraints.
val buildFromRegex: Topology.T -> Regex.REBuilder -> Regex.T list -> T

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

/// Returns a copy of the graph, restricted to nodes for a given preference
val restrict: T -> int -> T

/// Convert the constraint graph to the DOT format for visualization
val toDot: T -> string

/// Generate a png file for the constraint graph (requires graphviz dot utility)
val generatePNG: T -> string -> unit

  

module Reachable =
    /// All pairs reachability in the constraint graph
    val floydWarshall: T -> Map<CgState, Set<CgState>>

    /// Object to wrap the constraint graph and provide reachability info
    type AnnotatedCG =
        new: T -> AnnotatedCG
        member Cg: T
        member ReachInfo: Map<CgState, Set<CgState>>

    /// Check if src can reach dst while avoiding certain nodes
    val srcDstWithout: T -> CgState -> CgState -> (CgState -> bool) -> Direction -> bool

    /// Check if src can reach dst
    val srcDst: T -> CgState -> CgState -> Direction -> bool

    /// Find all destinations reachable from src while avoiding certain nodes
    val srcWithout: T -> CgState -> (CgState -> bool) -> Direction -> Set<CgState>

    /// Find all destinations reachable from src
    val src: T -> CgState -> Direction -> Set<CgState>

    /// Find all accepting preferences reachable from src while avoiding certain nodes
    val srcAcceptingWithout: T -> CgState -> (CgState -> bool) -> Direction -> Set<int> 

    /// Find all accepting preferences reachable from src
    val srcAccepting: T -> CgState -> Direction -> Set<int>

    /// Find all nodes reachable from src on a simple path
    val simplePathSrc: T -> CgState -> Direction -> Set<CgState>

    /// Find all nodes along some simple path from src to dst
    val alongSimplePathSrcDst: T -> CgState -> CgState -> Direction -> Set<CgState>

    /// Check if paths from n1 in cg1 are a superset of paths from n2 in cg2
    val supersetPaths: T * CgState -> T * CgState -> bool


module Minimize =
    /// Heuristic to remove edges not on any simple path
    val removeEdgesForDominatedNodes: T -> unit

    /// Remove nodes that will never result in an accepting state
    val removeNodesThatCantReachEnd: T -> unit

    /// Remove nodes that can't be reached from the start node without looping *)
    val removeNodesNotReachableOnSimplePath: T -> unit 

    /// Remove nodes that are on no path from start to end without looping
    val removeNodesNotOnAnySimplePathToEnd: T -> unit

    /// Get rid of nodes that can originate traffic but aren't accepting
    val delMissingSuffixPaths: T -> unit

    /// Remove obviously dead nodes
    val minimizeO0: T -> unit

    /// Remove obviously dead nodes and dominated edges
    val minimizeO1: T -> unit

    /// Remove obviously dead nodes, dominated edges, and nodes not on simple path
    val minimizeO2: T -> unit

    /// Fixpoint removal of nodes and edges, including nodes not on any simple path
    val minimizeO3: T -> unit


module Consistency =
    /// An explanation for why a policy is unimplementable with BGP
    type CounterExample = CgState * CgState

    /// Preference ranking for each router based on possible routes
    type Preferences = seq<CgState>

    /// Preferences for each internal router
    type Ordering = Map<string, Preferences>

    /// Conservative check if the BGP routers can make local decisions not knowing about failures
    /// Takes an optional file name for debugging intermediate information
    val findOrderingConservative: (T -> string -> Result<Ordering, CounterExample>)

    /// Exact check if BGP routes can make local decisions by enumerating failures
    /// Takes an optional file name for debugging intermediate information
    val findOrderingEnumerate: int -> (T -> string -> Result<Ordering, CounterExample>)