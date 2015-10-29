module CGraph

open Extension.Error 
open QuickGraph
open QuickGraph.Algorithms

type CgState = 
    {States: int array; 
     Accept: int option; 
     Topo: Topology.State}

type ConstraintGraph = 
    {Start: CgState; 
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}

exception PrefConsistencyException of CgState * CgState 
exception TopoConsistencyException of CgState * CgState

type ConsistencyViolation = 
    | PrefConsistency of CgState * CgState
    | TopoConsistency of CgState * CgState

type Preferences = (CgState * Set<int>) list

type Ordering = 
    Map<string, Preferences>


module Reach = 

    let floydWarshall (cg: ConstraintGraph) : Map<CgState, Set<CgState>> = 
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

    type AnnotatedCG(cg: ConstraintGraph) =
        let reachability = floydWarshall cg
        member this.Cg = cg
        member this.ReachInfo = reachability

    let srcDstWithout (cg: ConstraintGraph) src dst without =
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

    let srcWithout (cg: ConstraintGraph) src without =
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



let private copyGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG}

let private copyReverseGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG}


(* Polynomial-time heuristic for pruning edges that never lie on a 
   simple path. This looks for bidirectional edges between two nodes 
   X and Y, and checks if X -> Y can be removed *)
let private removeDeadEdgesHeuristic (cg: ConstraintGraph) = 
    let cgRev = copyReverseGraph cg
    cg.Graph.RemoveEdgeIf (fun (e: TaggedEdge<CgState,unit>) -> 
        let oes = cg.Graph.OutEdges e.Source
        let ies = cgRev.Graph.OutEdges e.Source

        let ie = Seq.tryFind (fun (ie: TaggedEdge<CgState,unit>) -> ie.Target = e.Target) ies
        match ie with 
        | None -> false 
        | Some ie ->
            let cantReachAccepting = Set.isEmpty (Reach.srcAcceptingWithout cg e.Target ((=) e.Source))
            let startCantReach = not (Reach.srcDstWithout cg cg.Start e.Source ((=) e.Target))
            cantReachAccepting || startCantReach
    ) |> ignore


(* This is a worst-case exponential time algorithm that marks and removes all nodes 
   to which there is no simple path to an accepting state. It does a depth-first search 
   that maintains a set of seen labels. To avoid exploring unnecessary paths, at each step 
   it checks if there are any reachable, unmarked states that don't go through an already 
   observed label *)
let private removeDeadStatesHeuristic (cg: ConstraintGraph) =
    let explored = ref 0
    let cantReach = ref (cg.Graph.Vertices |> Set.ofSeq)
    let rec search v seen = 
        explored := !explored + 1
        cantReach := Set.remove v !cantReach
        let exclude = (fun node -> node <> v && Set.contains node.Topo.Loc seen)
        let reachable = Reach.srcWithout cg v exclude
        let relevant = Set.exists (fun x -> Set.contains x reachable) !cantReach 
        if relevant then
            for e in cg.Graph.OutEdges v do
                let u = e.Target 
                if not (Set.contains u.Topo.Loc seen) then 
                    search u (Set.add u.Topo.Loc seen)
    search cg.Start Set.empty
    Set.iter (fun v -> cg.Graph.RemoveVertex v |> ignore) !cantReach

    (* Remove states that can't reach an accepting state *)
    let mutable deadNodes = Set.empty 
    for v in cg.Graph.Vertices do 
        if Set.isEmpty (Reach.srcAccepting cg v) then 
            deadNodes <- Set.add v deadNodes
    for v in deadNodes do 
        cg.Graph.RemoveVertex v |> ignore

    (* printfn "Number of nodes total: %A" cg.Graph.VertexCount
    printfn "Number of nodes explored: %A" !explored *)


let pruneHeuristic (cg: ConstraintGraph) = 
    removeDeadEdgesHeuristic cg 
    removeDeadStatesHeuristic cg


let pruneExact (cg: ConstraintGraph) = 
    let acg = Reach.AnnotatedCG(cg)
    let num_explored = ref 0
    let cantReachNodes = ref (acg.Cg.Graph.Vertices |> Set.ofSeq)
    let cantReachEdges = ref (acg.Cg.Graph.Edges |> Seq.map (fun e -> (e.Source, e.Target)) |> Set.ofSeq)
    let rec search v seenLocations seenNodes seenEdges =
        num_explored := !num_explored + 1
        if Option.isSome v.Accept then 
            cantReachNodes := Set.difference !cantReachNodes seenNodes
            cantReachEdges := Set.difference !cantReachEdges seenEdges
        for e in acg.Cg.Graph.OutEdges v do
            let u = e.Target 
            let notInPath = not (Set.contains u.Topo.Loc seenLocations)
            if notInPath then 
                search u (Set.add u.Topo.Loc seenLocations) (Set.add u seenNodes) (Set.add (v,u) seenEdges)
    search acg.Cg.Start Set.empty (Set.singleton acg.Cg.Start) Set.empty
    Set.iter (fun v -> acg.Cg.Graph.RemoveVertex v |> ignore) !cantReachNodes
    acg.Cg.Graph.RemoveEdgeIf (fun e -> Set.contains (e.Source, e.Target) !cantReachEdges) |> ignore


(* TODO: more efficient mutable data structures *)
let build (topo: Topology.T) (autos : Regex.Automata array) : ConstraintGraph = 

    let minPref x y = 
        match x, y with 
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b 
        | Some a, Some b -> Some (min a b)

    (* Graph topology information *)
    let alphabetIn, alphabetOut = Topology.alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let neighborMap = Topology.neighborMap topo

    (* Create the new constraint graph *)
    let graph = AdjacencyGraph<CgState, TaggedEdge<CgState,unit>>()
    let starting = Array.map (fun (x: Regex.Automata) -> x.q0) autos
    
    let newStart = 
        {States = starting; 
         Accept = None; 
         Topo = {Loc="start"; Typ = Topology.Start} }
    graph.AddVertex newStart |> ignore

    let mutable finished = Set.empty
    let mutable todo = Set.singleton newStart

    while not (Set.isEmpty todo) do
        let currState = Set.minElement todo 
        todo <- Set.remove currState todo
        graph.AddVertex currState |> ignore
        let {States=ss; Topo=t} = currState

        let neighbors = 
            if t.Typ = Topology.Start then 
                Set.filter Topology.isEndHostConnected alphabetAll 
            else Map.find t neighborMap

        for c in Set.intersect alphabetAll neighbors do
            let nextInfo = Array.init autos.Length (fun i -> 
                let g, v = autos.[i], ss.[i]
                let key = Map.findKey (fun (q,S) _ -> q = v && Set.contains c.Loc S) g.trans
                let newState = Map.find key g.trans 
                let accept = 
                    if (Topology.isEndHostConnected c) && (Set.contains newState g.F) then 
                        Some g.pref 
                    else None
                newState, accept
            )
            let nextStates, nextAccept = Array.unzip nextInfo
            let accept = Array.fold minPref None nextAccept
            let state = {States= nextStates; Accept=accept; Topo=c}
            graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
            if Set.contains state finished then ()
            else
                todo <- Set.add state todo
        finished <- Set.add currState finished
    {Start=newStart; Graph=graph}



let private prefConsistency (cg: ConstraintGraph) : Result<Ordering, ConsistencyViolation> =

    (* Custom sorting by reachable state preference ranges *)
    let comparer (_,x) (_,y) = 
        let minx, maxx = Set.minElement x, Set.maxElement y
        let miny, maxy = Set.minElement y, Set.maxElement y
        let cmp = compare minx miny
        if cmp = 0 then 
            compare (maxx - minx) (maxy - miny)
        else cmp
    
    (* Check well-formed preference ranges *)
    let rec aux ls = 
        match ls with
        | [] | [_] -> ls
        | ((vx,x) as hd)::(( (vy,y)::_) as tl) ->
            let maxx = Set.maxElement x 
            let miny = Set.minElement y
            if maxx > miny then
                raise (PrefConsistencyException (vx, vy))
            else 
                hd :: (aux tl)
    
    (* Map topology locations to a set of nodes/preferences *)
    let mutable acc = Map.empty
    for v in cg.Graph.Vertices do 
        let loc = v.Topo.Loc
        let ins = if Map.containsKey loc acc then Map.find loc acc else []
        let accepting = Reach.srcAccepting cg v
        acc <- Map.add loc ((v, accepting)::ins) acc 
    
    (* Ensure well-formedness and return the ordering. Raises an exception otherwise *) 
    let check _ v = aux (List.sortWith comparer v)

    (* Hide the exception behind a result type *)
    try Ok (Map.map check acc)
    with PrefConsistencyException (x,y) ->
        Err (PrefConsistency (x,y))


let private topoConsistency (cg: ConstraintGraph) (ord: Ordering) : Result<unit, ConsistencyViolation> =

    let checkFailures loc x y = 
        (* failing links from x disconnects y from start to accept state *)
        let es = Reach.edges cg x
        if not (Set.isEmpty es) && (Option.isSome x.Accept) then 
            let copy = copyGraph cg 
            for (u,v) in es do 
                copy.Graph.RemoveEdgeIf (fun e -> 
                    (e.Source.Topo.Loc = u.Topo.Loc && e.Target.Topo.Loc = v.Topo.Loc) || 
                    (e.Target.Topo.Loc = u.Topo.Loc && e.Source.Topo.Loc = v.Topo.Loc)
                ) |> ignore

            (* Check reachability for y *)
            if Reach.srcDst copy copy.Start y then
                if not (Set.isEmpty (Reach.srcAccepting copy y)) then 
                    raise (TopoConsistencyException (x,y))

    let rec aux loc prefs =
        match prefs with 
        | [] | [_] -> ()
        | (x,_)::(((y,_)::z) as tl) -> 
            checkFailures loc x y
            aux loc tl
    try
        Map.iter aux ord
        Ok ()
    with TopoConsistencyException(x,y) -> 
        Err (TopoConsistency (x,y))



let private genConfig (cg: ConstraintGraph) (ord: Ordering) : Config.T = 
    
    let compareLocThenPref (x,i1) (y,i2) = 
        let cmp = compare i1 i2
        if cmp = 0 then 
            compare x.Topo.Loc y.Topo.Loc
        else cmp

    let rec aux sorted = 
        match sorted with 
        | [] | [_] -> sorted
        | hd1::((hd2::z) as tl) ->
            let (x,i1) = hd1 
            let (y,i2) = hd2 
            if x.Topo.Loc = y.Topo.Loc then aux (hd1::z)
            else hd1 :: (aux tl)
    
    let cgRev = copyReverseGraph cg

    let neighborsIn v = 
        seq {for e in cgRev.Graph.OutEdges v do 
                if e.Target.Topo.Typ <> Topology.Start then yield e.Target}

    let neighborsOut v = 
        seq {for e in cg.Graph.OutEdges v do
                if e.Target.Topo.Typ <> Topology.Start then yield e.Target}


    let mutable config = Map.empty

    for entry in ord do 
        let mutable rules = []
        
        let loc = entry.Key 
        let prefs = entry.Value 
        
        let prefNeighborsIn = 
            prefs
            |> List.mapi (fun i (v,_) -> (neighborsIn v, i))
            |> List.map (fun (ns,i) -> Seq.map (fun n -> (n,i)) ns) 
            |> Seq.fold Seq.append Seq.empty 
            |> List.ofSeq
            |> List.sortWith compareLocThenPref
            |> aux
 
        let mutable lp = 99
        let mutable lastPref = None
        
        for v, pref in prefNeighborsIn do 

            match lastPref with 
            | Some p when pref = p -> () 
            | _ ->
                lastPref <- Some pref 
                lp <- lp + 1

            let unambiguous = 
                prefNeighborsIn 
                |> Set.ofList 
                |> Set.filter (fun (x,_) -> x.Topo.Loc = v.Topo.Loc) 
                |> Set.count 
                |> ((=) 1)

            let m = 
                if unambiguous then Config.Peer v.Topo.Loc 
                else Config.State (v.States, v.Topo.Loc)

            let a = 
                if lp = 100 then [] 
                else [Config.SetLP(lp)]

            rules <- {Config.Import = m; Config.Export = a}::rules
        
        config <- Map.add loc rules config

    config


(* Generate the BGP match/action rules that are guaranteed to 
   implement the user policy under all possible failure scenarios. 
   This function returns an intermediate representation (IR) for BGP policies *) 
let compile (cg: ConstraintGraph) : Result<Config.T, ConsistencyViolation> =
    match prefConsistency cg with 
    | Ok ord ->
        match topoConsistency cg ord with 
        | Ok _ -> Ok (genConfig cg ord)
        | Err(tc) -> Err(tc)
    | Err(pc) -> Err(pc)


(* Generate Graphviz DOT format output for visualization *)
let toDot (cg: ConstraintGraph) = 
    let onFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = ()

    let onFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
        let states = Array.map string v.Vertex.States |> String.concat ", "
        let location = v.Vertex.Topo.Loc.ToString()
        match v.Vertex.Accept with 
        | None -> 
            v.VertexFormatter.Label <- "(" + states + ", " + location + ")"
        | Some i ->
            v.VertexFormatter.Label <- "(" + states + ", " + location + "\npref=" + (string i)
            v.VertexFormatter.Shape <- Graphviz.Dot.GraphvizVertexShape.DoubleCircle
            v.VertexFormatter.Style <- Graphviz.Dot.GraphvizVertexStyle.Filled
            v.VertexFormatter.FillColor <- Graphviz.Dot.GraphvizColor.LightYellow

    let graphviz = Graphviz.GraphvizAlgorithm<CgState, TaggedEdge<CgState,unit>>(cg.Graph)
    graphviz.FormatEdge.Add(onFormatEdge)
    graphviz.FormatVertex.Add(onFormatVertex)
    graphviz.Generate()
   


(*
let compile (rs: Policy.Constraint list) : ConstraintGraph = 
    failwith "Todo"
*)