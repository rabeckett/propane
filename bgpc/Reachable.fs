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

let edges cg x = 
    let mutable es = Set.empty
    let mutable seen = Set.empty
    let mutable todo = Set.singleton x
    while not (Set.isEmpty todo) do 
        let current = Set.minElement todo 
        todo <- Set.remove current todo
        seen <- Set.add current seen
        for e in cg.Graph.OutEdges current do 
            es <- Set.add (e.Source, e.Target) es 
            if not (Set.contains e.Target seen) then 
                todo <- Set.add e.Target todo
    es