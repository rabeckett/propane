module Reachable

open CGraph
open QuickGraph
open QuickGraph.Algorithms


(* All pairs reachability in the constraint graph *)
let floydWarshall (cg: CGraph.T) : Map<CgState, Set<CgState>> = 
    let fw = ShortestPath.FloydWarshallAllShortestPathAlgorithm(cg.Graph, fun _ -> 1.0)
    fw.Compute ()
    let mutable reachability = Map.empty
    for src in cg.Graph.Vertices do 
        let mutable toDst = Set.singleton src
        for dst in cg.Graph.Vertices do 
            if fw.TryGetPath(src, dst, ref Seq.empty) then 
                toDst <- Set.add dst toDst
        reachability <- Map.add src toDst reachability
    reachability


(* Object to wrap the constraint graph and provide reachability info *)
type AnnotatedCG(cg: CGraph.T) =
    let reachability = floydWarshall cg
    member this.Cg = cg
    member this.ReachInfo = reachability


(* Check if src can reach dst while avoiding certain nodes *)
let srcDstWithout (cg: CGraph.T) src dst without =
    if without src || without dst then false 
    else if src = dst then true 
    else
        let numNodes = float cg.Graph.VertexCount
        let weight (e: TaggedEdge<CgState,unit>) = 
                if without e.Target then numNodes + 1.0 else 1.0
        let tryGetPaths = cg.Graph.ShortestPathsDijkstra((fun e -> weight e), src)
        let path = ref Seq.empty
        tryGetPaths.Invoke(dst, path) |> ignore
        match !path with 
        | null -> false 
        | _ ->
            let weight = Seq.fold (fun acc e -> acc + weight e) 0.0 !path
            weight <= numNodes


(* Check if src can reach dst *)
let srcDst cg src dst = 
    srcDstWithout cg src dst (fun _ -> false)


(* Find all destinations reachable from src while avoiding certain nodes *)
let srcWithout (cg: CGraph.T) src without =
    if without src then Set.empty
    else
        let numNodes = float cg.Graph.VertexCount
        let weight (e: TaggedEdge<CgState,unit>) = 
                if without e.Target then numNodes + 1.0 else 1.0
        let tryGetPaths = cg.Graph.ShortestPathsDijkstra((fun e -> weight e), src)
        let mutable reachable = Set.singleton src
        let path = ref Seq.empty
        for v in cg.Graph.Vertices do 
            tryGetPaths.Invoke(v, path) |> ignore
            match !path with 
            | null -> () 
            | _ ->
                let weight = Seq.fold (fun acc e -> acc + weight e) 0.0 !path
                if weight <= numNodes then
                    reachable <- Set.add v reachable
        reachable


(* Find all destinations reachable from src *)
let src cg src = 
    srcWithout cg src (fun _ -> false)


(* Find all accepting states reachable from src while avoiding certain nodes *)
let srcAcceptingWithout cg src without = 
    let aux acc cg = 
        match cg.Accept with 
        | None -> acc
        | Some i -> Set.add i acc
    srcWithout cg src without |> Set.fold aux Set.empty


(* Find all accepting states reachable from src *)
let srcAccepting cg src = 
    srcAcceptingWithout cg src (fun _ -> false)


(* Find all nodes reachable from src on a simple path *)
let simplePathSrc cg src = 
    let explored = ref 0
    let allNodes = cg.Graph.Vertices |> Set.ofSeq
    let cantReach = ref allNodes
    let rec search v seen = 
        explored := !explored + 1
        cantReach := Set.remove v !cantReach
        (* Stop if no unmarked node reachable without repeating location *)
        let exclude = (fun node -> node <> v && Set.contains node.Topo.Loc seen)
        let reachable = srcWithout cg v exclude
        let relevant = Set.exists (fun x -> Set.contains x reachable) !cantReach 
        if relevant then
            for e in cg.Graph.OutEdges v do
                let u = e.Target 
                if not (Set.contains u.Topo.Loc seen) then 
                    search u (Set.add u.Topo.Loc seen)
    search src Set.empty
    Set.difference allNodes !cantReach


(* Find all nodes along some simple path from src to dst *)
let alongSimplePathSrcDst cg src dst = 
    let num_explored = ref 0
    let allNodes = cg.Graph.Vertices |> Set.ofSeq
    let cantReach = ref allNodes
    let rec search v seenLocations seenNodes =
        num_explored := !num_explored + 1
        if v = cg.End then 
            cantReach := Set.difference !cantReach seenNodes
        (* Stop if can't reach the end state *)
        let exclude = (fun node -> node <> v && Set.contains node.Topo.Loc seenLocations)
        let reachable = srcWithout cg v exclude
        let seenUnmarked = not (Set.isEmpty (Set.intersect !cantReach seenNodes))
        let canReachUnmarked = Set.exists (fun v -> Set.contains v !cantReach) reachable
        if seenUnmarked || canReachUnmarked then
            let canReachDst = Set.contains dst reachable
            if canReachDst then
                for e in cg.Graph.OutEdges v do
                    let u = e.Target 
                    let notInPath = not (Set.contains u.Topo.Loc seenLocations)
                    if notInPath then 
                        search u (Set.add u.Topo.Loc seenLocations) (Set.add u seenNodes)
    search src Set.empty (Set.singleton cg.Start)
    Set.difference allNodes !cantReach