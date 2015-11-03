module Minimize
open CGraph

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