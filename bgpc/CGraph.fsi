module CGraph
open QuickGraph
open Extension.Error

type CgState = 
    {States: int array; 
     Accept: Set<int>; 
     Node: Topology.State}

type T = 
    {Start: CgState;
     End: CgState;
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>;
     Topo: Topology.T}

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
val preferences: T -> Set<int>

/// Returns the set of states that are attached to the end node
val acceptingStates: T -> Set<CgState>

/// Returns the set of locations that are attached to the end node
val acceptingLocations: T -> Set<string>

/// Returns the (outgoing) neighbors of a state in the graph
val neighbors: T -> CgState -> Set<CgState> 

/// Returns a copy of the graph, restricted to nodes for a given preference
val restrict: T -> int -> T

/// Convert the constraint graph to the DOT format for visualization
val toDot: T -> string
  

module Reachable =
    /// All pairs reachability in the constraint graph
    val floydWarshall: T -> Map<CgState, Set<CgState>>

    /// Object to wrap the constraint graph and provide reachability info
    type AnnotatedCG =
        new: T -> AnnotatedCG
        member Cg: T
        member ReachInfo: Map<CgState, Set<CgState>>

    /// Check if src can reach dst while avoiding certain nodes
    val srcDstWithout: T -> CgState -> CgState -> (CgState -> bool) -> bool

    /// Check if src can reach dst
    val srcDst: T -> CgState -> CgState -> bool

    /// Find all destinations reachable from src while avoiding certain nodes
    val srcWithout: T -> CgState -> (CgState -> bool) -> Set<CgState>

    /// Find all destinations reachable from src
    val src: T -> CgState -> Set<CgState>

    /// Find all accepting preferences reachable from src while avoiding certain nodes
    val srcAcceptingWithout: T -> CgState -> (CgState -> bool) -> Set<int> 

    /// Find all accepting preferences reachable from src
    val srcAccepting: T -> CgState -> Set<int>

    /// Find all nodes reachable from src on a simple path
    val simplePathSrc: T -> CgState -> Set<CgState>

    /// Find all nodes along some simple path from src to dst
    val alongSimplePathSrcDst: T -> CgState -> CgState -> Set<CgState>

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

    /// Heuristic to minimize the constraint graph
    val pruneHeuristic: T -> unit


module Consistency =
    /// An explanation for why a policy is unimplementable with BGP
    type CounterExample = CgState * CgState

    /// Preference ranking for each router based on possible routes
    type Preferences = seq<CgState>

    /// Preferences for each internal router
    type Ordering = Map<string, Preferences>

    /// Conservative check if the BGP routers can make local decisions not knowing about failures
    val findOrderingConservative: (T -> Result<Ordering, CounterExample>)

    /// Exact check if BGP routes can make local decisions by enumerating failures
    val findOrderingEnumerate: int -> (T -> Result<Ordering, CounterExample>)