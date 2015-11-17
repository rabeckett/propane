module Consistency

open Extension.Error
open CGraph
open QuickGraph
open QuickGraph.Algorithms

exception ConsistencyException of CgState * CgState 

type CounterExample = CgState * CgState

type Preferences = seq<CgState>

type Ordering = Map<string, Preferences>

let isPreferred restrict restrictRev x y =
    Set.forall (fun i -> 
        Set.forall (fun j -> 
            Set.exists (fun i' ->
                Reachable.supersetPaths (Map.find i' restrict, x) (Map.find j restrict, y) || 
                Reachable.supersetPaths (Map.find i' restrict, x) (Map.find j restrictRev, y)
            ) x.Accept ) y.Accept ) x.Accept

let findPrefAssignment restrict restrictRev nodes = 
    let g = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>()
    for n in nodes do 
        g.AddVertex n |> ignore
    (* Build a graph capturing preference constraints *)
    let edges = ref Set.empty
    for x in nodes do 
        for y in nodes do 
            if x <> y && isPreferred restrict restrictRev x y then 
                edges := Set.add (x,y) !edges
                edges := Set.add (y,x) !edges
                g.AddEdge (TaggedEdge(x, y, ())) |> ignore
    (* Check for incomparable nodes *)
    for x in g.Vertices do
        for y in g.Vertices do
            if x <> y then
                if not (Set.contains (x,y) !edges || Set.contains (y,x) !edges) then 
                    raise (ConsistencyException(x,y))
    (* Remove edges that don't constrain our choice *)
    let both = Set.filter (fun (x,y) -> Set.exists (fun (a,b) -> x=b && y=a) !edges) !edges
    g.RemoveEdgeIf (fun e -> Set.contains (e.Source, e.Target) both) |> ignore
    (* Pick an ordering that respects the contraints *)
    g.TopologicalSort ()

let addForLabel restrict restrictRev cg map l =
    if Map.containsKey l map then map
    else
        let nodes = Seq.filter (fun v -> v.Topo.Loc = l) cg.Graph.Vertices
        Map.add l (findPrefAssignment restrict restrictRev nodes) map

let findOrdering (cg: CGraph.T) : Result<Ordering, CounterExample> =
    let prefs = CGraph.preferences cg 
    let restrict = Set.fold (fun acc i -> Map.add i (CGraph.restrict cg i) acc) Map.empty prefs
    Map.iter (fun _ cg -> Minimize.removeNodesThatCantReachEnd cg) restrict
    let restrictRev = Map.map (fun _ cg -> CGraph.copyReverseGraph cg) restrict
    let labels = 
        cg.Graph.Vertices
        |> Seq.map (fun v -> v.Topo.Loc)
        |> Set.ofSeq 
    let map = ref Map.empty
    try 
        Ok(Set.fold (addForLabel restrict restrictRev cg) Map.empty labels)
    with ConsistencyException(x,y) ->
        Err( (x,y) )