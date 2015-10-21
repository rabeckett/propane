module ConstraintGraph

open QuickGraph
open QuickGraph.Algorithms


type NodeType = 
    | Start
    | Outside
    | Inside 
    | InsideHost 

type TopoState = 
    {loc: string; 
     typ: NodeType}

type Topology = BidirectionalGraph<TopoState,TaggedEdge<TopoState,unit>>

type CgState = 
    {states: int array; 
     accept: int option; 
     topo: TopoState}

type ConstraintGraph = 
    {start: CgState; 
     graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}

   
let unreachable() = failwith "unreachable"


(* determine the valid alphabet from the topology *)
let private alphabet (topo: Topology) = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.typ with 
        | Inside | InsideHost -> ain <- Set.add v ain
        | Outside -> aout <- Set.add v aout
        | Start -> unreachable()
    (ain, aout)

(* Find and remember all topological neighbors for each location *)
let private neighborMap (topo: Topology) = 
    let mutable nmap = Map.empty
    for v in topo.Vertices do
        let mutable adj = Set.empty 
        for e in topo.OutEdges v do 
            adj <- Set.add e.Target adj
        for e in topo.InEdges v do 
            adj <- Set.add e.Source adj
        nmap <- Map.add v adj nmap
    nmap


let copyGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.graph.Edges do newCG.AddEdge e |> ignore
    {start=cg.start; graph=newCG}

let copyReverseGraph (cg: ConstraintGraph) : ConstraintGraph = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {start=cg.start; graph=newCG}


type AnnotatedCG(cg: ConstraintGraph) =

    let floydWarshall () = 
        let fw = ShortestPath.FloydWarshallAllShortestPathAlgorithm(cg.graph, fun _ -> 1.0)
        fw.Compute ()
        let mutable reachability = Map.empty
        for src in cg.graph.Vertices do 
            let mutable toDst = Set.singleton src
            for dst in cg.graph.Vertices do 
                if fw.TryGetPath(src, dst, ref Seq.empty) then 
                    toDst <- Set.add dst toDst
            reachability <- Map.add src toDst reachability
        reachability

    let reachability = floydWarshall ()

    member this.cg = cg
    member this.ReachInfo = reachability

 

let reachableSrcDst (cg: ConstraintGraph) src dst = 
    if src = dst then true 
    else
        let tryGetPaths = cg.graph.ShortestPathsDijkstra((fun _ -> 1.0), src)
        tryGetPaths.Invoke(dst, ref Seq.empty) 


let reachableSrc (cg: ConstraintGraph) src = 
    let tryGetPaths = cg.graph.ShortestPathsDijkstra((fun _ -> 1.0), src)
    let mutable reachable = Set.singleton src
    for v in cg.graph.Vertices do 
        if tryGetPaths.Invoke(v, ref Seq.empty) then 
            reachable <- Set.add v reachable
    reachable


let reachableSrcAccepting cg src = 
    let aux acc cg = 
        match cg.accept with 
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
    for v in cg.graph.Vertices do 
        let oes = cg.graph.OutEdges v
        let ies = cgRev.graph.OutEdges v 
        if Seq.length oes = 1 && Seq.length ies = 1 then
            let oe = Seq.nth 0 oes 
            let ie = Seq.nth 0 ies
            if oe.Target = ie.Target then 
                deadEdges <- oe :: deadEdges
    for de in deadEdges do
        ignore (cg.graph.RemoveEdge de)
    
    let acg = AnnotatedCG(cg)
    let cantReach = ref (acg.cg.graph.Vertices |> Set.ofSeq)

    let rec search v seen = 
        cantReach := Set.remove v !cantReach
        for e in acg.cg.graph.OutEdges v do
            let u = e.Target 
            (* Optimization to avoid unnecessary exploration *)
            let relevant = Set.exists (fun x -> Set.contains x (Map.find u acg.ReachInfo)) !cantReach 
            let notInPath = not (Set.contains u.topo.loc seen)
            if relevant && notInPath then 
                search u (Set.add u.topo.loc seen)
                    
    search acg.cg.start Set.empty
    Set.iter (fun v -> acg.cg.graph.RemoveVertex v |> ignore) !cantReach

    (* Remove states that can't reach an accepting state *)
    let removeUnacceptableStates (cg: ConstraintGraph) = 
        let mutable deadNodes = Set.empty 
        for v in cg.graph.Vertices do 
            if Set.isEmpty (reachableSrcAccepting cg v) then 
                deadNodes <- Set.add v deadNodes
        for v in deadNodes do 
            cg.graph.RemoveVertex v |> ignore

    (* Old reachability information is no longer available after removing nodes *)
    removeUnacceptableStates cg 
    

(* TODO: more efficient mutable data structures *)
(* TODO: check for duplicate topology nodes *)
(* TODO: well defined in and out (fully connected inside) *)
let build (topo: Topology) (autos : Policy.Automata array) : ConstraintGraph = 

    (* Helper function to find best preference *)
    let minPref x y = 
        match x, y with 
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b 
        | Some a, Some b -> Some (min a b)

    let isEndHostConnected t = 
        match t.typ with 
        | InsideHost | Outside -> true
        | Start | Inside -> false

    (* Graph topology information *)
    let alphabetIn, alphabetOut = alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let neighborMap = neighborMap topo

    (* Create the new constraint graph *)
    let graph = AdjacencyGraph<CgState, TaggedEdge<CgState,unit>>()

    (* Create the new initial node and add to the graph *)
    let starting = Array.map (fun (x: Policy.Automata) -> x.q0) autos
    let newStart = {states=starting; accept=None; topo={loc="start"; typ=Start}}
    graph.AddVertex newStart |> ignore

    (* Explore reachable nodes and add to constraint graph *)
    let mutable finished = Set.empty
    let mutable todo = Set.singleton newStart

    while not (Set.isEmpty todo) do
        (* Extract and add the next todo node *)
        let currState = Set.minElement todo 
        todo <- Set.remove currState todo
        graph.AddVertex currState |> ignore
        let {states=ss; topo=t} = currState

        (* should never fail *)
        let neighbors = 
            if t.typ = Start then 
                Set.filter isEndHostConnected alphabetAll 
            else Map.find t neighborMap

        for c in Set.intersect alphabetAll neighbors do
            let nextInfo = Array.init autos.Length (fun i -> 
                let g = autos.[i]
                let v = ss.[i]
                let key = Map.findKey (fun (q,S) _ -> q = v && Set.contains c.loc S) g.trans
                let newState = Map.find key g.trans 
                let accept = 
                    if (isEndHostConnected c) && (Set.contains newState g.F) then 
                        Some g.pref 
                    else None
                newState, accept
            )
            let nextStates, nextAccept = Array.unzip nextInfo
            let accept = Array.fold minPref None nextAccept                
            let state = {states= nextStates; accept=accept; topo=c}

            graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
            
            if Set.contains state finished then ()
            else
                todo <- Set.add state todo

        finished <- Set.add currState finished

    {start=newStart; graph=graph}


type Ordering = Map<string, (CgState * Set<int>) list>

exception ViolatesPrefConsistency of CgState * CgState
exception ViolatesTopoConsistency


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
                raise (ViolatesPrefConsistency (vx, vy))
            else 
                hd :: (aux tl)

    let mutable acc = Map.empty
    for v in cg.graph.Vertices do 
        let loc = v.topo.loc
        let ins = if Map.containsKey loc acc then Map.find loc acc else []
        acc <- Map.add loc ((v, reachableSrcAccepting cg v)::ins) acc 
          
    let check _ v = aux (List.sortWith comparer v)    
    Map.map check acc


let topoConsistency (cg: ConstraintGraph) (ord: Ordering) = 
    failwith "todo"


let genRules (cg: ConstraintGraph) (ord: Ordering) = 
    let cgRev = copyReverseGraph cg
    
    let mutable ruleMap = Map.empty 
    for v in cgRev.graph.Vertices do 
        let rcvEdges = cgRev.graph.OutEdges v
        for re in rcvEdges do
            let u = re.Target
            let rcvState = u.states

            let localpref = 
                Map.find v.topo.loc ord
                |> List.findIndex (fun (v',_) -> v' = v)
                |> (+) 100
            
            printfn "Receive from: %A, %A" rcvState u.topo.loc
            printfn "Update to:    %A, %A" v.states v.topo.loc
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
        | ViolatesPrefConsistency(v,u) -> 
            printfn "Nodes %A and %A violate pref consistency" v u 
        | ViolatesTopoConsistency -> 
            failwith "Todo"


(*
let compile (rs: Policy.Constraint list) : ConstraintGraph = 
    failwith "Todo"
*)