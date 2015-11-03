module CGraph
open QuickGraph


type CgState = 
    {States: int array; 
     Accept: int option; 
     Topo: Topology.State}

type T = 
    {Start: CgState;
     End: CgState;
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}

/// Make a shallow copy of the graph. Does not clone node values.
val copyGraph: T -> T

/// Make a shallow copy of the graph, and reverses all edges
val copyReverseGraph: T -> T

/// Constructs a new, product automaton from the topology and a 
/// collection of regular expression automata for different route preferences.
/// All paths through the product graph a valid topology paths that satisfy
/// one or more of the regular path constraints.
val build: Topology.T -> Regex.Automaton array -> T

/// Convert the constraint graph to the DOT format for visualization
val toDot: T -> string
   
