module CGraph
open QuickGraph
open QuickGraph.Algorithms
open Extension.Error

type CgState = 
    {States: int array; 
     Accept: Set<int>; 
     Node: Topology.State}
     override this.ToString() = 
        "(State=" + (List.ofArray this.States).ToString() + ", Loc=" + this.Node.Loc + ")"

type T = 
    {Start: CgState;
     End: CgState;
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>
     Topo: Topology.T}

let copyGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End; Topo=cg.Topo}

let copyReverseGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End; Topo=cg.Topo}

let buildFromAutomata (topo: Topology.T) (autos : Regex.Automaton array) : T = 
    let alphabetIn, alphabetOut = Topology.alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let graph = AdjacencyGraph<CgState, TaggedEdge<CgState,unit>>()
    let starting = Array.map (fun (x: Regex.Automaton) -> x.q0) autos
    let newStart = {States = starting; Accept = Set.empty; Node = {Loc="start"; Typ = Topology.Start} }
    graph.AddVertex newStart |> ignore
    let mutable finished = Set.empty
    let mutable todo = Set.singleton newStart
    while not (Set.isEmpty todo) do
        let currState = Set.minElement todo 
        todo <- Set.remove currState todo
        graph.AddVertex currState |> ignore
        let {States=ss; Node=t} = currState
        let neighbors = 
            if t.Typ = Topology.Start then 
                Set.filter Topology.canOriginateTraffic alphabetAll 
            else 
                topo.OutEdges t 
                |> Seq.map (fun e -> e.Target)
                |> Set.ofSeq 
        for c in Set.intersect alphabetAll neighbors do
            let nextInfo = Array.init autos.Length (fun i -> 
                let g, v = autos.[i], ss.[i]
                let key = Map.findKey (fun (q,S) _ -> q = v && Set.contains c.Loc S) g.trans
                let newState = Map.find key g.trans 
                let accept = 
                    if (Topology.canOriginateTraffic c) && (Set.contains newState g.F) then 
                        Set.singleton (i+1)
                    else Set.empty
                newState, accept
            )
            let nextStates, nextAccept = Array.unzip nextInfo
            let accept = Array.fold Set.union Set.empty nextAccept
            let state = {States=nextStates; Accept=accept; Node=c}
            graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
            if Set.contains state finished then ()
            else 
                todo <- Set.add state todo
        finished <- Set.add currState finished
    let newEnd = {States = [||]; Accept = Set.empty; Node = {Loc="end"; Typ = Topology.End}}
    graph.AddVertex newEnd |> ignore
    let accepting = Seq.filter (fun v -> not (Set.isEmpty v.Accept)) graph.Vertices
    Seq.iter (fun v -> graph.AddEdge(TaggedEdge(v, newEnd, ())) |> ignore) accepting
    {Start=newStart; Graph=graph; End=newEnd; Topo=topo}

let buildFromRegex (topo: Topology.T) (reb: Regex.REBuilder) (res: Regex.T list) : T = 
    res 
    |> List.map (fun r -> reb.MakeDFA (reb.Rev r))
    |> Array.ofList
    |> buildFromAutomata topo

let preferences (cg: T) : Set<int> = 
    let mutable all = Set.empty
    for v in cg.Graph.Vertices do 
        all <- Set.union all v.Accept
    all

let acceptingStates (cg: T) : Set<CgState> =
    cg.Graph.Vertices
    |> Seq.filter (fun (v: CgState) -> not v.Accept.IsEmpty)
    |> Set.ofSeq

let acceptingLocations (cg: T) : Set<string> = 
    acceptingStates cg
    |> Set.map (fun v -> v.Node.Loc)

let neighbors (cg: T) (state: CgState) : Set<CgState> =
    cg.Graph.OutEdges state
    |> Seq.map (fun e -> e.Target) 
    |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
    |> Set.ofSeq

let restrict (cg: T) (i: int) : T = 
    if Set.contains i (preferences cg) then 
        let copy = copyGraph cg
        copy.Graph.RemoveVertexIf (fun v -> 
            not v.Accept.IsEmpty && not (Set.contains i v.Accept)
        ) |> ignore
        copy
    else cg

let toDot (cg: T) : string = 
    let onFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = ()
    let onFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
        let states = Array.map string v.Vertex.States |> String.concat ", "
        let location = v.Vertex.Node.Loc.ToString()
        match v.Vertex.Node.Typ with 
        | Topology.Start -> v.VertexFormatter.Label <- "Start"
        | Topology.End -> v.VertexFormatter.Label <- "End"
        | _ ->
            if Set.isEmpty v.Vertex.Accept then 
                v.VertexFormatter.Label <- "(" + states + ", " + location + ")"
            else
                v.VertexFormatter.Label <- "(" + states + ", " + location + ")" + "\npref=" + (v.Vertex.Accept.ToString ())
                v.VertexFormatter.Shape <- Graphviz.Dot.GraphvizVertexShape.DoubleCircle
                v.VertexFormatter.Style <- Graphviz.Dot.GraphvizVertexStyle.Filled
                v.VertexFormatter.FillColor <- Graphviz.Dot.GraphvizColor.LightYellow
    let graphviz = Graphviz.GraphvizAlgorithm<CgState, TaggedEdge<CgState,unit>>(cg.Graph)
    graphviz.FormatEdge.Add(onFormatEdge)
    graphviz.FormatVertex.Add(onFormatVertex)
    graphviz.Generate()


module Reachable =

    let floydWarshall (cg: T) : Map<CgState, Set<CgState>> = 
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

    type AnnotatedCG(cg: T) =
        let reachability = floydWarshall cg
        member this.Cg = cg
        member this.ReachInfo = reachability

    let srcDstWithout (cg: T) src dst without =
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

    let srcWithout (cg: T) src without =
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
            Set.union cg.Accept acc
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
            let exclude = (fun node -> node <> v && Set.contains node.Node.Loc seen)
            let reachable = srcWithout cg v exclude
            let relevant = Set.exists (fun x -> Set.contains x reachable) !cantReach 
            if relevant then
                for e in cg.Graph.OutEdges v do
                    let u = e.Target 
                    if not (Set.contains u.Node.Loc seen) then 
                        search u (Set.add u.Node.Loc seen)
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
            let exclude = (fun node -> node <> v && Set.contains node.Node.Loc seenLocations)
            let reachable = srcWithout cg v exclude
            let seenUnmarked = not (Set.isEmpty (Set.intersect !cantReach seenNodes))
            let canReachUnmarked = Set.exists (fun v -> Set.contains v !cantReach) reachable
            if seenUnmarked || canReachUnmarked then
                let canReachDst = Set.contains dst reachable
                if canReachDst then
                    for e in cg.Graph.OutEdges v do
                        let u = e.Target 
                        let notInPath = not (Set.contains u.Node.Loc seenLocations)
                        if notInPath then 
                            search u (Set.add u.Node.Loc seenLocations) (Set.add u seenNodes)
        search src Set.empty (Set.singleton cg.Start)
        Set.difference allNodes !cantReach

    /// Checks if n1 in graph cg1 simulates n2 in cg2
    let supersetPaths (cg1, n1) (cg2, n2) : bool =
        let add k v map = 
            match Map.tryFind k map with 
            | None -> Map.add k (Set.singleton v) map
            | Some vs -> Map.add k (Set.add v vs) map

        let addAll k vs map = Set.fold (fun acc v -> add k v acc) map vs

        let merge x y = Map.fold (fun acc k v -> addAll k v acc) x y

        let remainsSuperset b = 
            Map.forall (fun k v -> 
                not (Map.exists (fun k' v' -> 
                    (k.Node.Loc = k'.Node.Loc && k <> k') &&
                    (Set.intersect v v' |> Set.isEmpty |> not) ) b)) b

        let stepNodeNode n1 n2 = 
            let neighbors1 = neighbors cg1 n1
            let neighbors2 = neighbors cg2 n2
            let nchars1 = Set.map (fun v -> v.Node.Loc) neighbors1
            let nchars2 = Set.map (fun v -> v.Node.Loc) neighbors2
            if not (Set.isSuperset nchars1 nchars2) then
                (* printfn "PROBLEM" *)
                None
            else
                let newBisim = ref Map.empty
                let common = Set.intersect nchars1 nchars2 
                Set.iter (fun c ->
                    let v1 = Set.filter (fun v -> v.Node.Loc = c) neighbors1 |> Set.minElement
                    let v2 = Set.filter (fun v -> v.Node.Loc = c) neighbors2 |> Set.minElement
                    newBisim := add v1 v2 !newBisim
                ) common
                Some !newBisim
 
        let stepNodeNodes n1 n2s = 
            Set.fold (fun acc n2 ->
                match acc, stepNodeNode n1 n2 with 
                | None, _ | _, None -> None 
                | Some acc, Some x -> Some (merge acc x)
            ) (Some Map.empty) n2s

        let updateBisim b n1 n2s = 
            match b, stepNodeNodes n1 n2s with 
            | None, _ | _, None -> None
            | Some b, Some x -> Some (merge b x)

        let rec iter n bisim = 
            (* printfn "Bisimulation step %d: %A" n bisim *)
            match n with 
            | 0 -> true
            | _ ->
                if Map.isEmpty bisim then true else
                if not (remainsSuperset bisim) then false else 
                let b = Map.fold updateBisim (Some Map.empty) bisim
                (* printfn "New map: %A" b *)
                match b with 
                | None -> false
                | Some b -> iter (n-1) b

        if n1.Node.Loc <> n2.Node.Loc then false 
        else
            let bisim = Map.add n1 (Set.singleton n2) Map.empty
            let steps = max cg1.Graph.VertexCount cg2.Graph.VertexCount 
            iter steps bisim


module Minimize =

    (* TODO: use faster algorithms for this *)
    let removeEdgesForDominatedNodes (cg: T) = 
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



    let removeNodesThatCantReachEnd (cg: T) = 
        cg.Graph.RemoveVertexIf(fun v -> Topology.isTopoNode v.Node && not (Reachable.srcDst cg v cg.End)) |> ignore

    let removeNodesNotReachableOnSimplePath (cg: T) =
        let canReach = Reachable.simplePathSrc cg cg.Start
        cg.Graph.RemoveVertexIf (fun v -> Topology.isTopoNode v.Node && not (Set.contains v canReach)) |> ignore

    let removeNodesNotOnAnySimplePathToEnd (cg: T) = 
        let canReach = Reachable.alongSimplePathSrcDst cg cg.Start cg.End
        cg.Graph.RemoveVertexIf (fun v -> Topology.isTopoNode v.Node && not (Set.contains v canReach)) |> ignore

    let pruneHeuristic (cg: T) =
        (* TODO: Need good story on single starting node *)
        let starting = neighbors cg cg.Start
        cg.Graph.RemoveVertexIf (fun v -> 
            v.Node.Typ = Topology.InsideOriginates && 
            v.Accept.IsEmpty && 
            not (Set.contains v starting)
        ) |> ignore
        (* Remove useless edges and nodes *)
        System.IO.File.WriteAllText("temp.dot", toDot cg)

        removeEdgesForDominatedNodes cg
        removeNodesNotReachableOnSimplePath cg 
        removeNodesThatCantReachEnd cg
        
        removeNodesNotOnAnySimplePathToEnd cg


module Consistency = 

    exception ConsistencyException of CgState * CgState
    type CounterExample =  CgState * CgState
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
            let r = restrict cg i 
            Minimize.removeNodesThatCantReachEnd r
            Map.add i r acc
        Set.fold aux Map.empty prefs

    let simulate _ (restrict, restrictRev) (x,y) (i,j) =
        let restrict_i = copyGraph (Map.find i restrict)
        let restrict_j = copyGraph (Map.find j restrict)
        (* Check if the restricted graph contains the node we care about *)
        if not (restrict_i.Graph.ContainsVertex x && restrict_j.Graph.ContainsVertex y) then 
            false
        else
            let restrict_irev = copyGraph (Map.find i restrictRev)
            let restrict_jrev = copyGraph (Map.find j restrictRev)
            (* Remove same labelled nodes *)
            restrict_i.Graph.RemoveVertexIf (fun v -> v.Node.Loc = x.Node.Loc && v <> x) |> ignore
            restrict_irev.Graph.RemoveVertexIf (fun v -> v.Node.Loc = x.Node.Loc && v <> x) |> ignore
            restrict_j.Graph.RemoveVertexIf (fun v -> v.Node.Loc = y.Node.Loc && v <> y) |> ignore
            restrict_jrev.Graph.RemoveVertexIf (fun v -> v.Node.Loc = y.Node.Loc && v <> y) |> ignore
            (* Remove nodes that appear 'above' for the more preferred, to avoid considering simple paths *)
            let reach = Reachable.src restrict_irev x |> Set.map (fun v -> v.Node.Loc)
            restrict_i.Graph.RemoveVertexIf (fun v -> v <> x && Set.contains v.Node.Loc reach) |> ignore
            (* Check if the more preferred simulates the less preferred *)
            (Reachable.supersetPaths (restrict_i, x) (restrict_j, y) || 
             Reachable.supersetPaths (restrict_i, x) (restrict_jrev, y))


    let failedGraph (cg: T) (failures: Topology.Failure.FailType list) : T =
        let failed = copyGraph cg
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

    let enumerate n (cg: T) (restrict, restrictRev) (x,y) (i,j) = 
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
         
    let findOrdering f (cg: T) : Result<Ordering, CounterExample> =
        let prefs = preferences cg 
        let rs = restrictedGraphs cg prefs
        let rsRev = Map.map (fun _ cg -> copyReverseGraph cg) rs
        (* Map.iter (fun i cg -> System.IO.File.WriteAllText("restricted" + i.ToString() + ".dot", CGraph.toDot cg)) restrict *)
        let labels = 
            cg.Graph.Vertices
            |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
            |> Seq.map (fun v -> v.Node.Loc)
            |> Set.ofSeq 
        try Ok(Set.fold (addForLabel f (rs, rsRev) cg) Map.empty labels)
        with ConsistencyException(x,y) ->
            Err((x,y) )

    let findOrderingEnumerate n = findOrdering (enumerate n)

    let findOrderingConservative = findOrdering simulate