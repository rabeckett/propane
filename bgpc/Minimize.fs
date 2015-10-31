module Minimize

open CGraph
open QuickGraph


(* Heuristic to remove edges not on any simple path *)
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


(* Remove nodes that will never result in an accepting state *)
let removeNodesThatCantReachEnd (cg: CGraph.T) = 
    cg.Graph.RemoveVertexIf(fun v -> not (Reachable.srcDst cg v cg.End)) |> ignore


(* Remove nodes that can't be reached from the start node without looping *)
let removeNodesNotReachableOnSimplePath (cg: CGraph.T) =
    let canReach = Reachable.simplePathSrc cg cg.Start
    cg.Graph.RemoveVertexIf (fun v -> not (Set.contains v canReach)) |> ignore


(* Remove nodes that are on no path from start to end without looping *)
let removeNodesNotOnAnySimplePathToEnd (cg: CGraph.T) = 
    let canReach = Reachable.alongSimplePathSrcDst cg cg.Start cg.End
    cg.Graph.RemoveVertexIf (fun v -> not (Set.contains v canReach)) |> ignore


(* Heuristic to minimize the constraint graph *)
let pruneHeuristic (cg: CGraph.T) = 
    removeEdgesForDominatedNodes cg 
    removeNodesNotReachableOnSimplePath cg
    removeNodesThatCantReachEnd cg
