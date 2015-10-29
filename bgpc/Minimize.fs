module Minimize

open CGraph
open QuickGraph


let removeEdgesForDominatedNodes (cg: CGraph.T) = 
    let cgRev = copyReverseGraph cg
    cg.Graph.RemoveEdgeIf (fun (e: TaggedEdge<CgState,unit>) -> 
        let oes = cg.Graph.OutEdges e.Source
        let ies = cgRev.Graph.OutEdges e.Source
        let ie = Seq.tryFind (fun (ie: TaggedEdge<CgState,unit>) -> ie.Target = e.Target) ies
        match ie with 
        | None -> false 
        | Some ie ->
            let cantReachAccepting = Set.isEmpty (Reachable.srcAcceptingWithout cg e.Target ((=) e.Source))
            let startCantReach = not (Reachable.srcDstWithout cg cg.Start e.Source ((=) e.Target))
            cantReachAccepting || startCantReach
    ) |> ignore


let removeNodesThatCantReachEnd (cg: CGraph.T) = 
    cg.Graph.RemoveVertexIf(fun v -> not (Reachable.srcDst cg v cg.End)) |> ignore

let removeNodesNotReachableOnSimplePath (cg: CGraph.T) =
    let explored = ref 0
    let cantReach = ref (cg.Graph.Vertices |> Set.ofSeq)
    let rec search v seen = 
        explored := !explored + 1
        cantReach := Set.remove v !cantReach
        let exclude = (fun node -> node <> v && Set.contains node.Topo.Loc seen)
        let reachable = Reachable.srcWithout cg v exclude
        let relevant = Set.exists (fun x -> Set.contains x reachable) !cantReach 
        if relevant then
            for e in cg.Graph.OutEdges v do
                let u = e.Target 
                if not (Set.contains u.Topo.Loc seen) then 
                    search u (Set.add u.Topo.Loc seen)
    search cg.Start Set.empty
    Set.iter (fun v -> cg.Graph.RemoveVertex v |> ignore) !cantReach

let removeNodesNotOnAnySimplePathToEnd (cg: CGraph.T) = 
    let acg = Reachable.AnnotatedCG(cg)
    let num_explored = ref 0
    let cantReachNodes = ref (acg.Cg.Graph.Vertices |> Set.ofSeq)
    let rec search v seenLocations seenNodes seenEdges =
        num_explored := !num_explored + 1
        if v = cg.End then 
            cantReachNodes := Set.difference !cantReachNodes seenNodes
        for e in acg.Cg.Graph.OutEdges v do
            let u = e.Target 
            let notInPath = not (Set.contains u.Topo.Loc seenLocations)
            if notInPath then 
                search u (Set.add u.Topo.Loc seenLocations) (Set.add u seenNodes) (Set.add (v,u) seenEdges)
    search acg.Cg.Start Set.empty (Set.singleton acg.Cg.Start) Set.empty
    Set.iter (fun v -> acg.Cg.Graph.RemoveVertex v |> ignore) !cantReachNodes


let pruneHeuristic (cg: CGraph.T) = 
    removeEdgesForDominatedNodes cg 
    removeNodesNotReachableOnSimplePath cg
    removeNodesThatCantReachEnd cg
