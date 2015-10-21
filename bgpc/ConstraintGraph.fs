module ConstraintGraph

open QuickGraph
open QuickGraph.Algorithms


type CgState = 
    {States: int array; 
     Accept: int option; 
     Topo: Topology.State}

type ConstraintGraph = 
    {Start: CgState; 
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}

   

(* Get the alphabet from the topology *)
let private alphabet (topo: Topology.T) = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.Typ with 
        | Topology.Inside -> ain <- Set.add v ain
        | Topology.InsideHost -> ain <- Set.add v ain
        | Topology.Outside -> aout <- Set.add v aout
        | Topology.Start -> Assert.unreachable()
    (ain, aout)

(* Find and remember all topological neighbors for each location *)
let private neighborMap (topo: Topology.T) = 
    let mutable nmap = Map.empty
    for v in topo.Vertices do
        let mutable adj = Set.empty 
        for e in topo.OutEdges v do 
            adj <- Set.add e.Target adj
        for e in topo.InEdges v do 
            adj <- Set.add e.Source adj
        nmap <- Map.add v adj nmap
    nmap

(* Make a copy of the constraint graph *)
let copyGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG}

(* Make a copy of the constraint graph with all directed edges reversed *)
let copyReverseGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG}

(* Compute all-pairs reachability *)
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

(* Attach reachability information to the graph *)
type AnnotatedCG(cg: ConstraintGraph) =
    let reachability = floydWarshall cg
    member this.cg = cg
    member this.ReachInfo = reachability

(* Check if src can reach dst *)
let reachableSrcDst (cg: ConstraintGraph) src dst = 
    if src = dst then true 
    else
        let tryGetPaths = cg.Graph.ShortestPathsDijkstra((fun _ -> 1.0), src)
        tryGetPaths.Invoke(dst, ref Seq.empty) 

(* Find all locations reachable from src *)
let reachableSrc (cg: ConstraintGraph) src = 
    let tryGetPaths = cg.Graph.ShortestPathsDijkstra((fun _ -> 1.0), src)
    let mutable reachable = Set.singleton src
    for v in cg.Graph.Vertices do 
        if tryGetPaths.Invoke(v, ref Seq.empty) then 
            reachable <- Set.add v reachable
    reachable

(* Find all accepting states reachable from src *)
let reachableSrcAccepting cg src = 
    let aux acc cg = 
        match cg.Accept with 
        | None -> acc
        | Some i -> Set.add i acc
    reachableSrc cg src |> Set.fold aux Set.empty


(* Remove states and edges that are effectively dead.
    1. Circular links
    2. Node can't reach an accepting state 
    3. No path can reach a node without loops *)

let removeDeadStates (cg: ConstraintGraph) = 
    (* Remove obvious loop transitions *)
    let cgRev = copyReverseGraph(cg)
    let mutable deadEdges = []
    for v in cg.Graph.Vertices do 
        let oes = cg.Graph.OutEdges v
        let ies = cgRev.Graph.OutEdges v 
        if Seq.length oes = 1 && Seq.length ies = 1 then
            let oe = Seq.nth 0 oes 
            let ie = Seq.nth 0 ies
            if oe.Target = ie.Target then 
                deadEdges <- oe :: deadEdges
    for de in deadEdges do
        ignore (cg.Graph.RemoveEdge de)
    
    (* Remove nodes that can't be reached on non-loop path *)
    let acg = AnnotatedCG(cg)
    let cantReach = ref (acg.cg.Graph.Vertices |> Set.ofSeq)
    let rec search v seen = 
        cantReach := Set.remove v !cantReach
        for e in acg.cg.Graph.OutEdges v do
            let u = e.Target 
            (* Optimization to avoid unnecessary exploration *)
            let relevant = Set.exists (fun x -> Set.contains x (Map.find u acg.ReachInfo)) !cantReach 
            let notInPath = not (Set.contains u.Topo.Loc seen)
            if relevant && notInPath then 
                search u (Set.add u.Topo.Loc seen)                 
    search acg.cg.Start Set.empty
    Set.iter (fun v -> acg.cg.Graph.RemoveVertex v |> ignore) !cantReach

    (* Remove states that can't reach an accepting state *)
    let removeUnacceptableStates (cg: ConstraintGraph) = 
        let mutable deadNodes = Set.empty 
        for v in cg.Graph.Vertices do 
            if Set.isEmpty (reachableSrcAccepting cg v) then 
                deadNodes <- Set.add v deadNodes
        for v in deadNodes do 
            cg.Graph.RemoveVertex v |> ignore
    removeUnacceptableStates cg 
    

(* TODO: more efficient mutable data structures *)
(* TODO: check for duplicate topology nodes *)
(* TODO: well defined in and out (fully connected inside) *)
let build (topo: Topology.T) (autos : Regex.Automata array) : ConstraintGraph = 

    (* Helper function to find best preference *)
    let minPref x y = 
        match x, y with 
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b 
        | Some a, Some b -> Some (min a b)

    let isEndHostConnected (t: Topology.State) = 
        match t.Typ with 
        | Topology.InsideHost -> true 
        | Topology.Outside -> true
        | Topology.Inside -> false
        | Topology.Start -> false

    (* Graph topology information *)
    let alphabetIn, alphabetOut = alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let neighborMap = neighborMap topo

    (* Create the new constraint graph *)
    let graph = AdjacencyGraph<CgState, TaggedEdge<CgState,unit>>()

    (* Create the new initial node and add to the graph *)
    let starting = Array.map (fun (x: Regex.Automata) -> x.q0) autos
    
    let newStart = 
        {States = starting; 
         Accept = None; 
         Topo = {Loc="start"; Typ = Topology.Start} }

    graph.AddVertex newStart |> ignore

    (* Explore reachable nodes and add to constraint graph *)
    let mutable finished = Set.empty
    let mutable todo = Set.singleton newStart

    while not (Set.isEmpty todo) do
        (* Extract and add the next todo node *)
        let currState = Set.minElement todo 
        todo <- Set.remove currState todo
        graph.AddVertex currState |> ignore
        let {States=ss; Topo=t} = currState

        (* should never fail *)
        let neighbors = 
            if t.Typ = Topology.Start then 
                Set.filter isEndHostConnected alphabetAll 
            else Map.find t neighborMap

        for c in Set.intersect alphabetAll neighbors do
            let nextInfo = Array.init autos.Length (fun i -> 
                let g = autos.[i]
                let v = ss.[i]
                let key = Map.findKey (fun (q,S) _ -> q = v && Set.contains c.Loc S) g.trans
                let newState = Map.find key g.trans 
                let accept = 
                    if (isEndHostConnected c) && (Set.contains newState g.F) then 
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


type Ordering = Map<string, (CgState * Set<int>) list>

exception ViolatesPrefConsistencyException of CgState * CgState
exception ViolatesTopoConsistencyException


let prefConsistency (cg: ConstraintGraph) : Ordering =
    
    let comparer (_,x) (_,y) = 
        let minx, maxx = Set.minElement x, Set.maxElement y
        let miny, maxy = Set.minElement y, Set.maxElement y
        let cmp = compare minx miny
        if cmp = 0 then 
            compare (maxx - minx) (maxy - miny)
        else cmp
    
    let rec aux ls = 
        match ls with
        | [] | [_] -> ls
        | ((vx,x) as hd)::(( (vy,y)::_) as tl) ->
            let maxx = Set.maxElement x 
            let miny = Set.minElement y
            if maxx > miny then
                raise (ViolatesPrefConsistencyException (vx, vy))
            else 
                hd :: (aux tl)

    let mutable acc = Map.empty
    for v in cg.Graph.Vertices do 
        let loc = v.Topo.Loc
        let ins = if Map.containsKey loc acc then Map.find loc acc else []
        acc <- Map.add loc ((v, reachableSrcAccepting cg v)::ins) acc 
          
    let check _ v = aux (List.sortWith comparer v)    
    Map.map check acc


let topoConsistency (cg: ConstraintGraph) (ord: Ordering) = 
    failwith "todo"


let genRules (cg: ConstraintGraph) (ord: Ordering) = 
    let cgRev = copyReverseGraph cg
    
    let mutable ruleMap = Map.empty 
    for v in cgRev.Graph.Vertices do 
        let rcvEdges = cgRev.Graph.OutEdges v
        for re in rcvEdges do
            let u = re.Target
            let rcvState = u.States

            let localpref = 
                Map.find v.Topo.Loc ord
                |> List.findIndex (fun (v',_) -> v' = v)
                |> (+) 100
            
            printfn "Receive from: %A, %A" rcvState u.Topo.Loc
            printfn "Update to:    %A, %A" v.States v.Topo.Loc
            printfn "Local-Pref:   %A\n" localpref

            (* let adEdges = cg.graph.OutEdges v
            for ae in adEdges do
                let u = ae.Target
                
                if u.topo.typ <> Start then
                    printfn "  Send to: %A, %A" u.states u.topo.loc *)

let compile (cg: ConstraintGraph) =
    try 
        let ord = prefConsistency cg 
        (* topoConsistency cg ord |> ignore *)
        genRules cg ord 
    with 
        | ViolatesPrefConsistencyException(v,u) -> 
            printfn "Nodes %A and %A violate pref consistency" v u 
        | ViolatesTopoConsistencyException -> 
            failwith "Todo"


(* Generate Graphviz DOT format output *)
let toDot (cg: ConstraintGraph) = 
    let onFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = ()

    let onFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
        let states = Array.map string v.Vertex.States |> String.concat ", "
        let location = v.Vertex.Topo.Loc.ToString()
        match v.Vertex.Accept with 
        | None -> 
            v.VertexFormatter.Label <- "(" + states + ", " + location + ")"
        | Some i ->
            v.VertexFormatter.Label <- "(" + states + ", " + location + ")\npref=" + (string i)
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