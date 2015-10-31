module CGraph

open QuickGraph


(* Constraint graph state tracks automaton states, topo location, and prefs *)
type CgState = 
    {States: int array; 
     Accept: int option; 
     Topo: Topology.State}

(* Constraint graph type with dedicated start and end nodes *)
type T = 
    {Start: CgState;
     End: CgState;
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}


(* Make a new copy of the constraint graph *)
let copyGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End}

(* Make a copy of the constraint graph with edges reversed *)
let copyReverseGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End}


(* Build the constraint graph from the topology and compiled query DFAs *)
let build (topo: Topology.T) (autos : Regex.Automata array) : T = 

    let minPref x y = 
        match x, y with 
        | None, None -> None
        | Some a, None -> Some a
        | None, Some b -> Some b 
        | Some a, Some b -> Some (min a b)

    let alphabetIn, alphabetOut = Topology.alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let neighborMap = Topology.neighborMap topo

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
                Set.filter Topology.canOriginateTraffic alphabetAll 
            else Map.find t neighborMap

        for c in Set.intersect alphabetAll neighbors do
            let nextInfo = Array.init autos.Length (fun i -> 
                let g, v = autos.[i], ss.[i]
                let key = Map.findKey (fun (q,S) _ -> q = v && Set.contains c.Loc S) g.trans
                let newState = Map.find key g.trans 
                let accept = 
                    if (Topology.canOriginateTraffic c) && (Set.contains newState g.F) then 
                        Some (i+1)
                    else None
                newState, accept
            )
            let nextStates, nextAccept = Array.unzip nextInfo
            let accept = Array.fold minPref None nextAccept
            let state = {States=nextStates; Accept=accept; Topo=c}
            graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
           
            if Set.contains state finished then ()
            else todo <- Set.add state todo

        finished <- Set.add currState finished

    let newEnd = 
        {States = [||];
         Accept = None;
         Topo = {Loc="end"; Typ = Topology.End}}

    graph.AddVertex newEnd |> ignore

    let accepting = Seq.filter (fun v -> v.Accept <> None) graph.Vertices
    Seq.iter (fun v -> graph.AddEdge(TaggedEdge(v, newEnd, ())) |> ignore) accepting

    {Start=newStart; Graph=graph; End=newEnd}


(* Generate Graphviz DOT format output for visualization *)
let toDot (cg: T) : string = 
    let onFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = ()

    let onFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
        let states = Array.map string v.Vertex.States |> String.concat ", "
        let location = v.Vertex.Topo.Loc.ToString()
        match v.Vertex.Topo.Typ with 
        | Topology.Start -> v.VertexFormatter.Label <- "Start"
        | Topology.End -> v.VertexFormatter.Label <- "End"
        | _ ->
            match v.Vertex.Accept with 
            | None -> 
                v.VertexFormatter.Label <- "(" + states + ", " + location + ")"
            | Some i ->
                v.VertexFormatter.Label <- "(" + states + ", " + location + ")" + "\npref=" + (string i)
                v.VertexFormatter.Shape <- Graphviz.Dot.GraphvizVertexShape.DoubleCircle
                v.VertexFormatter.Style <- Graphviz.Dot.GraphvizVertexStyle.Filled
                v.VertexFormatter.FillColor <- Graphviz.Dot.GraphvizColor.LightYellow

    let graphviz = Graphviz.GraphvizAlgorithm<CgState, TaggedEdge<CgState,unit>>(cg.Graph)
    graphviz.FormatEdge.Add(onFormatEdge)
    graphviz.FormatVertex.Add(onFormatVertex)
    graphviz.Generate()
   