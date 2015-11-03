module Reachable

open CGraph
open QuickGraph
open QuickGraph.Algorithms


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

type AnnotatedCG(cg: CGraph.T) =
    let reachability = floydWarshall cg
    member this.Cg = cg
    member this.ReachInfo = reachability

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

let srcDst cg src dst = 
    srcDstWithout cg src dst (fun _ -> false)

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

let src cg src = 
    srcWithout cg src (fun _ -> false)

let srcAcceptingWithout cg src without = 
    let aux acc cg = 
        match cg.Accept with 
        | None -> acc
        | Some i -> Set.add i acc
    srcWithout cg src without |> Set.fold aux Set.empty

let srcAccepting cg src = 
    srcAcceptingWithout cg src (fun _ -> false)

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


let supersetPaths (cg1, n1) (cg2, n2) : bool =
    let add k v map = 
        match Map.tryFind k map with 
        | None -> Map.add k (Set.singleton v) map
        | Some vs -> Map.add k (Set.add v vs) map

    let addAll k vs map = 
        Set.fold (fun acc v -> add k v acc) map vs

    let merge x y = 
        Map.fold (fun acc k v -> addAll k v acc) x y

    let remainsSuperset b = 
        Map.fold (fun acc k v -> 
            acc && not (Map.exists (fun k' v' -> 
                (k.Topo.Loc = k'.Topo.Loc && k <> k') &&
                (Set.intersect v v' |> Set.isEmpty |> not) ) b)) true b

    let stepNodeNode n1 n2 = 
        let neighbors1 = 
            cg1.Graph.OutEdges n1 
            |> Seq.map (fun e -> e.Target) 
            |> Seq.filter (fun v -> Topology.isTopoNode v.Topo)
            |> Set.ofSeq
        let neighbors2 = 
            cg2.Graph.OutEdges n2 
            |> Seq.map (fun e -> e.Target) 
            |> Seq.filter (fun v -> Topology.isTopoNode v.Topo)
            |> Set.ofSeq
        let nchars1 = Set.map (fun v -> v.Topo.Loc) neighbors1
        let nchars2 = Set.map (fun v -> v.Topo.Loc) neighbors2
        if not (Set.isSuperset nchars1 nchars2) then 
            None
        else
            let newBisim = ref Map.empty
            let common = Set.intersect nchars1 nchars2 
            Set.iter (fun c ->
                let v1 = Set.filter (fun v -> v.Topo.Loc = c) neighbors1 |> Set.minElement
                let v2 = Set.filter (fun v -> v.Topo.Loc = c) neighbors2 |> Set.minElement
                newBisim := add v1 v2 !newBisim
            ) common
            Some !newBisim
 
    let stepNodeNodes n1 n2s = 
        Set.fold (fun acc n2 ->
            match acc, stepNodeNode n1 n2 with 
            | None, _ | _, None -> None 
            | Some acc, Some x -> Some (merge acc x)
        ) (Some Map.empty) n2s

    let step b n1 n2s = 
        match b, stepNodeNodes n1 n2s with 
        | None, _ | _, None -> None
        | Some b, Some x -> Some (merge b x)

    let rec iter n bisim = 
        match n with 
        | 0 -> true 
        | _ ->
            if Map.isEmpty bisim then true else
            if not (remainsSuperset bisim) then false else 
            let b = Map.fold step (Some Map.empty) bisim
            match b with 
            | None -> false
            | Some b -> iter (n-1) b

    if n1.Topo.Loc <> n2.Topo.Loc then false 
    else
        let bisim = Map.add n1 (Set.singleton n2) Map.empty
        let steps = max cg1.Graph.VertexCount cg2.Graph.VertexCount 
        iter steps bisim

