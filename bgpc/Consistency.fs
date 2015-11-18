module Consistency

open Extension.Error
open CGraph
open QuickGraph
open QuickGraph.Algorithms

exception ConsistencyException of CgState * CgState 

type CounterExample = CgState * CgState

type Preferences = seq<CgState>

type Ordering = Map<string, Preferences>

type Constraints = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>


let isPreferred (restrict,restrictRev) (x,y) (reachX,reachY) =
    let subsumes i' j =
        (Reachable.supersetPaths (Map.find i' restrict, x) (Map.find j restrict, y) || 
         Reachable.supersetPaths (Map.find i' restrict, x) (Map.find j restrictRev, y))
    Set.forall (fun j -> 
        (Set.exists (fun i' -> i' <= j && subsumes i' j) reachX) ) reachY

let checkIncomparableNodes (g: Constraints) edges = 
    for x in g.Vertices do
        for y in g.Vertices do
            if x <> y && not (Set.contains (x,y) edges || Set.contains (y,x) edges) then
                raise (ConsistencyException(x,y))

let removeUnconstrainedEdges (g: Constraints) edges =
    let both = Set.filter (fun (x,y) -> Set.exists (fun (a,b) -> x=b && y=a) edges) edges
    g.RemoveEdgeIf (fun e -> Set.contains (e.Source, e.Target) both) |> ignore

let getOrdering (g: Constraints) edges =
    checkIncomparableNodes g edges
    removeUnconstrainedEdges g edges
    g.TopologicalSort ()

let getReachabilityMap cg nodes = 
    let mutable reachMap = Map.empty
    for n in nodes do
        reachMap <- Map.add n (Reachable.srcAccepting cg n) reachMap
    reachMap

let addPrefConstraints (g: Constraints) r nodes reachMap =
    let mutable edges = Set.empty
    for x in nodes do
        for y in nodes do
            let reachX = Map.find x reachMap
            let reachY = Map.find y reachMap
            let isPref = isPreferred r (x,y) (reachX,reachY)
            if x <> y && isPref then
                printfn "%A is preferred to %A" x y
                edges <- Set.add (x,y) edges
                g.AddEdge (TaggedEdge(x, y, ())) |> ignore
    g, edges

let encodeConstraints cg r nodes =
    let g = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>()
    for n in nodes do 
        g.AddVertex n |> ignore
    let reachMap = getReachabilityMap cg nodes
    addPrefConstraints g r nodes reachMap

let findPrefAssignment r cg nodes = 
    let g, edges = encodeConstraints cg r nodes
    getOrdering g edges

let addForLabel r cg map l =
    if not (Map.containsKey l map) then 
        let nodes = Seq.filter (fun v -> v.Node.Loc = l) cg.Graph.Vertices
        Map.add l (findPrefAssignment r cg nodes) map
    else map

let restrictedGraphs cg prefs =
    let aux acc i =
        let r = CGraph.restrict cg i 
        Minimize.removeNodesThatCantReachEnd r
        Map.add i r acc
    Set.fold aux Map.empty prefs

let findOrdering (cg: CGraph.T) : Result<Ordering, CounterExample> =
    let prefs = CGraph.preferences cg 
    let rs = restrictedGraphs cg prefs
    let rsRev = Map.map (fun _ cg -> CGraph.copyReverseGraph cg) rs
    (* Map.iter (fun i cg -> System.IO.File.WriteAllText("restricted" + i.ToString() + ".dot", CGraph.toDot cg)) restrict *)
    let labels = 
        cg.Graph.Vertices
        |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
        |> Seq.map (fun v -> v.Node.Loc)
        |> Set.ofSeq 
    try Ok(Set.fold (addForLabel (rs, rsRev) cg) Map.empty labels)
    with ConsistencyException(x,y) ->
        Err( (x,y) )
