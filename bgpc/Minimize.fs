module Minimize
open CGraph
open QuickGraph

(* TODO: use faster algorithms for this *)
let removeEdgesForDominatedNodes (cg: CGraph.T) = 
    let cgRev = copyReverseGraph cg
    cg.Graph.RemoveEdgeIf (fun (e: TaggedEdge<CgState,unit>) -> 
        let oes = cg.Graph.OutEdges e.Source
        let ies = cgRev.Graph.OutEdges e.Source
        let ie = Seq.tryFind (fun (ie: TaggedEdge<CgState,unit>) -> ie.Target = e.Target) ies
        match ie with 
        | None -> false 
        | Some ie ->
            let cantReachAccepting = not (Reachable.srcDstWithout cg e.Target cg.End ((=) e.Source))
            let startCantReach = not (Reachable.srcDstWithout cg cg.Start e.Source ((=) e.Target))
            cantReachAccepting || startCantReach
    ) |> ignore

let removeNodesThatCantReachEnd (cg: CGraph.T) = 
    cg.Graph.RemoveVertexIf(fun v -> not (Reachable.srcDst cg v cg.End)) |> ignore

let removeNodesNotReachableOnSimplePath (cg: CGraph.T) =
    let canReach = Reachable.simplePathSrc cg cg.Start
    cg.Graph.RemoveVertexIf (fun v -> not (Set.contains v canReach)) |> ignore

let removeNodesNotOnAnySimplePathToEnd (cg: CGraph.T) = 
    let canReach = Reachable.alongSimplePathSrcDst cg cg.Start cg.End
    cg.Graph.RemoveVertexIf (fun v -> not (Set.contains v canReach)) |> ignore

let pruneHeuristic (cg: CGraph.T) = 
    removeEdgesForDominatedNodes cg
    removeNodesNotReachableOnSimplePath cg
    removeNodesThatCantReachEnd cg
    (* removeNodesNotOnAnySimplePathToEnd cg *)
