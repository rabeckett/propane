module Reachable
open CGraph

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
