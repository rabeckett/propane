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

let isPreferred f cg (restrict,restrictRev) (x,y) (reachX,reachY) =
    let subsumes i j =
        f cg (restrict, restrictRev) (x,y) (i,j)
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

let addPrefConstraints f cg (g: Constraints) r nodes reachMap =
    let mutable edges = Set.empty
    for x in nodes do
        for y in nodes do
            let reachX = Map.find x reachMap
            let reachY = Map.find y reachMap
            if x <> y && (isPreferred f cg r (x,y) (reachX,reachY)) then
                edges <- Set.add (x,y) edges
                g.AddEdge (TaggedEdge(x, y, ())) |> ignore
    g, edges

let encodeConstraints f cg r nodes =
    let g = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>()
    for n in nodes do 
        g.AddVertex n |> ignore
    let reachMap = getReachabilityMap cg nodes
    addPrefConstraints f cg g r nodes reachMap

let findPrefAssignment f r cg nodes = 
    let g, edges = encodeConstraints f cg r nodes
    getOrdering g edges

let addForLabel f r cg map l =
    if not (Map.containsKey l map) then 
        let nodes = Seq.filter (fun v -> v.Node.Loc = l) cg.Graph.Vertices
        Map.add l (findPrefAssignment f r cg nodes) map
    else map

let restrictedGraphs cg prefs =
    let aux acc i =
        let r = CGraph.restrict cg i 
        Minimize.removeNodesThatCantReachEnd r
        Map.add i r acc
    Set.fold aux Map.empty prefs

let simulate _ (restrict, restrictRev) (x,y) (i,j) =
        (Reachable.supersetPaths (Map.find i restrict, x) (Map.find j restrict, y) || 
         Reachable.supersetPaths (Map.find i restrict, x) (Map.find j restrictRev, y))

let failedGraph (cg: CGraph.T) (failures: Topology.Failure.FailType list) : CGraph.T =
    let failed = CGraph.copyGraph cg
    let rec aux acc fs =
        let (vs,es) = acc 
        match fs with
        | [] -> acc
        | (Topology.Failure.NodeFailure s)::tl ->
            aux (s.Loc::vs, es) tl
        | (Topology.Failure.LinkFailure s)::tl ->
            aux (vs, (s.Source.Loc, s.Target.Loc)::(s.Target.Loc, s.Source.Loc)::es) tl
    let (failedNodes, failedEdges) = aux ([],[]) failures
    failed.Graph.RemoveVertexIf (fun v -> List.exists ((=) v.Node.Loc) failedNodes) |> ignore
    failed.Graph.RemoveEdgeIf (fun e -> List.exists ((=) (e.Source.Node.Loc, e.Target.Node.Loc)) failedEdges) |> ignore
    failed

let enumerate n (cg: CGraph.T) (restrict, restrictRev) (x,y) (i,j) = 
    let gx = Map.find i restrict
    let gy = Map.find j restrict 
    let failCombos = Topology.Failure.allFailures n cg.Topo
    Seq.forall (fun fails ->
        let failedX = failedGraph gx fails
        let failedY = failedGraph gy fails
        not((Set.contains x (Reachable.simplePathSrc failedX cg.Start)) &&
           (not ( Set.contains x (Reachable.alongSimplePathSrcDst failedX cg.Start cg.End))) &&
           (Set.contains y (Reachable.alongSimplePathSrcDst failedY cg.Start cg.End)))
    ) failCombos
         
let findOrdering f (cg: CGraph.T) : Result<Ordering, CounterExample> =
    let prefs = CGraph.preferences cg 
    let rs = restrictedGraphs cg prefs
    let rsRev = Map.map (fun _ cg -> CGraph.copyReverseGraph cg) rs
    (* Map.iter (fun i cg -> System.IO.File.WriteAllText("restricted" + i.ToString() + ".dot", CGraph.toDot cg)) restrict *)
    let labels = 
        cg.Graph.Vertices
        |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
        |> Seq.map (fun v -> v.Node.Loc)
        |> Set.ofSeq 
    try Ok(Set.fold (addForLabel f (rs, rsRev) cg) Map.empty labels)
    with ConsistencyException(x,y) ->
        Err( (x,y) )


let findOrderingEnumerate n = findOrdering (enumerate n)

let findOrderingConservative = findOrdering simulate
