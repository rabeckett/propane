module CGraph
open QuickGraph

type CgState = 
    {States: int array; 
     Accept: Set<int>; 
     Topo: Topology.State}

type T = 
    {Start: CgState;
     End: CgState;
     Graph: AdjacencyGraph<CgState, TaggedEdge<CgState, unit>>}

let copyGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End}

let copyReverseGraph (cg: T) : T = 
    let newCG = QuickGraph.AdjacencyGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End}

let buildFromAutomata (topo: Topology.T) (autos : Regex.Automaton array) : T = 
    let alphabetIn, alphabetOut = Topology.alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let graph = AdjacencyGraph<CgState, TaggedEdge<CgState,unit>>()
    let starting = Array.map (fun (x: Regex.Automaton) -> x.q0) autos
    let newStart = {States = starting; Accept = Set.empty; Topo = {Loc="start"; Typ = Topology.Start} }
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
            let isValid = c.Typ = Topology.InsideOriginates && (Set.isEmpty accept)
            let notInitial = (currState.Topo.Typ <> Topology.Start)
            if isValid && notInitial then ()
            else
                let state = {States=nextStates; Accept=accept; Topo=c}
                graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
                if Set.contains state finished then ()
                else 
                    todo <- Set.add state todo
        finished <- Set.add currState finished
    let newEnd = {States = [||]; Accept = Set.empty; Topo = {Loc="end"; Typ = Topology.End}}
    graph.AddVertex newEnd |> ignore
    let accepting = Seq.filter (fun v -> not (Set.isEmpty v.Accept)) graph.Vertices
    Seq.iter (fun v -> graph.AddEdge(TaggedEdge(v, newEnd, ())) |> ignore) accepting
    {Start=newStart; Graph=graph; End=newEnd}

let buildFromRegex (topo: Topology.T) (reb: Regex.REBuilder) (res: Regex.T list) : T = 
    res 
    |> List.map (fun r -> reb.MakeDFA (reb.Rev r))
    |> Array.ofList
    |> buildFromAutomata topo

let inline preferences (cg: T) : Set<int> = 
    let mutable all = Set.empty
    for v in cg.Graph.Vertices do 
        all <- Set.union all v.Accept
    all

let restrict (cg: T) (i: int) : T = 
    if Set.contains i (preferences cg) then 
        let copy = copyGraph cg
        copy.Graph.RemoveVertexIf (fun v -> 
            not (Set.isEmpty v.Accept) && not (Set.contains i v.Accept)
        ) |> ignore
        copy
    else cg

let toDot (cg: T) : string = 
    let onFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = ()
    let onFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
        let states = Array.map string v.Vertex.States |> String.concat ", "
        let location = v.Vertex.Topo.Loc.ToString()
        match v.Vertex.Topo.Typ with 
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
   