module CGraph

open Common.Error
open Common.Debug
open System.Collections.Generic
open System.Diagnostics
open QuickGraph
open QuickGraph.Algorithms
open QuickGraph.Algorithms

[<CustomEquality; CustomComparison>]
type CgState =
    {Id: int;
     States: int array; 
     Accept: Set<int>; 
     Node: Topology.State}

    override this.ToString() = 
       "(Id=" + string this.Id + ", State=" + (List.ofArray this.States).ToString() + ", Loc=" + this.Node.Loc + ")"

    override x.Equals(other) =
       match other with
        | :? CgState as y -> (x.Id = y.Id)
        | _ -> false
 
    override x.GetHashCode() = hash x.Id

    interface System.IComparable with
        member x.CompareTo other =
          match other with
          | :? CgState as y -> x.Id - y.Id
          | _ -> failwith "cannot compare values of different types"

type CgStateTmp =
    {TStates: int array; 
     TAccept: Set<int>; 
     TNode: Topology.State}

type T = 
    {Start: CgState;
     End: CgState;
     Graph: BidirectionalGraph<CgState, TaggedEdge<CgState, unit>>
     Topo: Topology.T}

type Direction = Up | Down

let copyGraph (cg: T) : T = 
    let newCG = QuickGraph.BidirectionalGraph()
    for v in cg.Graph.Vertices do 
        newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        newCG.AddEdge e |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End; Topo=cg.Topo}

let copyReverseGraph (cg: T) : T = 
    let newCG = QuickGraph.BidirectionalGraph() 
    for v in cg.Graph.Vertices do newCG.AddVertex v |> ignore
    for e in cg.Graph.Edges do
        let e' = TaggedEdge(e.Target, e.Source, ())
        newCG.AddEdge e' |> ignore
    {Start=cg.Start; Graph=newCG; End=cg.End; Topo=cg.Topo}
   
let index ((graph, topo, startNode, endNode): BidirectionalGraph<CgStateTmp, TaggedEdge<CgStateTmp,unit>> * Topology.T * CgStateTmp * CgStateTmp) =
    let newCG = QuickGraph.BidirectionalGraph()
    let nstart = {Id=0; Node=startNode.TNode; States=startNode.TStates; Accept=startNode.TAccept}
    let nend = {Id=1; Node=endNode.TNode; States=endNode.TStates; Accept=endNode.TAccept}
    newCG.AddVertex nstart |> ignore
    newCG.AddVertex nend |> ignore
    let mutable i = 2
    let mutable idxMap = 
        Map.ofList [((nstart.Node.Loc, nstart.States), nstart); ((nend.Node.Loc, nend.States), nend)]
    for v in graph.Vertices do
        if Topology.isTopoNode v.TNode then
            let newv = {Id=i; Node=v.TNode; States = v.TStates; Accept=v.TAccept}
            i <- i + 1
            idxMap <- Map.add (v.TNode.Loc, v.TStates) newv idxMap
            newCG.AddVertex newv |> ignore
    for e in graph.Edges do
        let v = e.Source 
        let u = e.Target
        let x = Map.find (v.TNode.Loc, v.TStates) idxMap
        let y = Map.find (u.TNode.Loc, u.TStates) idxMap
        newCG.AddEdge (TaggedEdge(x,y,())) |> ignore
    {Start=nstart; Graph=newCG; End=nend; Topo=topo}

let getTransitions autos =
    let aux (auto: Regex.Automaton) = 
        Map.fold (fun acc (q1,S) q2 ->
            Set.fold (fun acc s ->
                Map.add (q1,s) q2 acc) acc S
        ) Map.empty auto.trans
    Array.map aux autos

let buildFromAutomata (topo: Topology.T) (autos : Regex.Automaton array) : T =
    if not (Topology.isWellFormed topo) then
        raise Topology.InvalidTopologyException
    let alphabetIn, alphabetOut = Topology.alphabet(topo)
    let alphabetAll = Set.union alphabetIn alphabetOut
    let transitions = getTransitions autos
    let graph = BidirectionalGraph<CgStateTmp, TaggedEdge<CgStateTmp,unit>>()
    let starting = Array.map (fun (x: Regex.Automaton) -> x.q0) autos
    let newStart = {TStates = starting; TAccept = Set.empty; TNode = {Loc="start"; Typ = Topology.Start} }
    graph.AddVertex newStart |> ignore
    let marked = HashSet(HashIdentity.Structural)
    let todo = Queue()
    todo.Enqueue newStart
    while todo.Count > 0 do
        let currState = todo.Dequeue()
        if not (marked.Contains currState) then 
            marked.Add currState |> ignore
            let {TStates=ss; TNode=t} = currState
            let adj = 
                if t.Typ = Topology.Start then 
                    Set.filter Topology.canOriginateTraffic alphabetAll 
                else 
                    topo.OutEdges t 
                    |> Seq.map (fun e -> e.Target)
                    |> Set.ofSeq
            let adj = if t.Typ = Topology.Unknown then Set.add t adj else adj
            for c in Set.intersect alphabetAll adj do
                let nextInfo = Array.init autos.Length (fun i ->
                    let g, v = autos.[i], ss.[i]
                    let newState = Map.find (v,c.Loc) transitions.[i]
                    let accept =
                        if (Topology.canOriginateTraffic c) && (Set.contains newState g.F) 
                        then Set.singleton (i+1)
                        else Set.empty
                    newState, accept)
                let nextStates, nextAccept = Array.unzip nextInfo
                let accept = Array.fold Set.union Set.empty nextAccept
                let state = {TStates=nextStates; TAccept=accept; TNode=c}
                graph.AddVertex state |> ignore
                graph.AddEdge(TaggedEdge(currState, state, ())) |> ignore
                todo.Enqueue state 
    let newEnd = {TStates = [||]; TAccept = Set.empty; TNode = {Loc="end"; Typ = Topology.End}}
    graph.AddVertex newEnd |> ignore
    let accepting = Seq.filter (fun v -> not (Set.isEmpty v.TAccept)) graph.Vertices
    Seq.iter (fun v -> graph.AddEdge(TaggedEdge(v, newEnd, ())) |> ignore) accepting
    index (graph, topo, newStart, newEnd)


let inline preferences (cg: T) : Set<int> = 
    let mutable all = Set.empty
    for v in cg.Graph.Vertices do 
        all <- Set.union all v.Accept
    all

let inline acceptingStates (cg: T) : Set<CgState> =
    cg.Graph.Vertices
    |> Seq.filter (fun (v: CgState) -> not v.Accept.IsEmpty)
    |> Set.ofSeq

let inline acceptingLocations (cg: T) : Set<string> = 
    acceptingStates cg
    |> Set.map (fun v -> v.Node.Loc)

let inline isRealNode (state: CgState) : bool =
    Topology.isTopoNode state.Node

let inline neighbors (cg: T) (state: CgState) =
    cg.Graph.OutEdges state
    |> Seq.map (fun e -> e.Target) 

let inline neighborsIn (cg: T) (state: CgState) = 
    cg.Graph.InEdges state
    |> Seq.map (fun e -> e.Source)

let inline isRepeatedOut (cg: T) (state: CgState) =
    let ns = neighbors cg state
    (state.Node.Typ = Topology.Unknown) &&
    (Seq.exists (fun n -> n = state) ns)

let restrict (cg: T) (i: int) = 
    if Set.contains i (preferences cg) then 
        let copy = copyGraph cg
        copy.Graph.RemoveVertexIf (fun v -> 
            not (v.Accept.IsEmpty) && 
            not (Set.exists (fun i' -> i' <= i) v.Accept)
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

let generatePNG (cg: T) (file: string) : unit =
    System.IO.File.WriteAllText(file + ".dot", toDot cg)
    let p = new Process()
    p.StartInfo.FileName <- "dot"
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.Arguments <- "-Tpng " + file + ".dot -o " + file + ".png" 
    p.StartInfo.CreateNoWindow <- true
    p.Start() |> ignore
    p.WaitForExit();


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

    let dfs (cg: T) (source: CgState) direction : seq<CgState> = seq { 
        let f = if direction = Up then neighborsIn else neighbors
        let s = Stack()
        let marked = HashSet()
        s.Push source
        while s.Count > 0 do 
            let v = s.Pop()
            if not (marked.Contains v) then 
                ignore (marked.Add v)
                yield v
                for w in f cg v do 
                    s.Push w }

    let srcWithout (cg: T) source without direction =
        let f = if direction = Up then neighborsIn else neighbors
        let s = Stack()
        let mutable marked = Set.empty
        s.Push source
        while s.Count > 0 do 
            let v = s.Pop()
            if not (marked.Contains v) && not (without v) then 
                marked <- marked.Add v
                for w in f cg v do 
                    s.Push w
        marked

    let inline srcDstWithout (cg: T) source sink without direction = 
        if without sink || without source then false
        else Set.contains sink (srcWithout cg source without direction)

    let inline src (cg: T) (source: CgState) direction : Set<CgState> =
        srcWithout cg source (fun _ -> false) direction

    let inline srcDst (cg: T) source sink direction = 
        srcDstWithout cg source sink (fun _ -> false) direction

    let inline srcAcceptingWithout cg src without direction = 
        srcWithout cg src without direction 
        |> Set.fold (fun acc cg -> Set.union cg.Accept acc) Set.empty

    let inline srcAccepting cg src direction = 
        srcAcceptingWithout cg src (fun _ -> false) direction

(*
    let simplePathSrcRepeat cg src canRepeat direction =
        let f = if direction = Up then neighborsIn else neighbors 
        let explored = ref 0
        let allNodes = cg.Graph.Vertices |> Set.ofSeq
        let cantReach = ref allNodes
        let rec search v seenLocs seenNodes = 
            explored := !explored + 1
            cantReach := Set.remove v !cantReach
            (* Stop if no unmarked node reachable without repeating location *)
            let exclude = (fun node -> node <> v && not (canRepeat node) && Set.contains node.Node.Loc seenLocs)
            let reachable = srcWithout cg v exclude Down
            let relevant = Set.exists (fun x -> Set.contains x reachable) !cantReach 
            if relevant then
                for u in f cg v do
                    let seenNodeBefore = Set.contains u seenNodes
                    let seenLocBefore = Set.contains u.Node.Loc seenLocations
                    let shouldContinue = not seenLocBefore || (canRepeat u && not seenNodeBefore)
                    if not (Set.contains u.Node.Loc seenLocs) then 
                        (* let newSeen = if canRepeat u then seen else (Set.add u.Node.Loc seen) *)
                        search u (Set.add u.Node.Loc seenLocs) (Set.add u seenNodes)
        search src Set.empty (Set.singleton cg.Start)
        Set.difference allNodes !cantReach

    let simplePathSrc cg src direction = 
        let canRepeat v = (v.Node.Typ = Topology.Unknown)
        simplePathSrcRepeat cg src canRepeat direction  *)

    let alongSimplePathSrcDstRepeat cg src dst canRepeat direction = 
        let f = if direction = Up then neighborsIn else neighbors
        let num_explored = ref 0
        let allNodes = cg.Graph.Vertices |> Set.ofSeq
        let cantReach = ref allNodes
        let rec search v seenLocations seenNodes =
            num_explored := !num_explored + 1
            if v = cg.End then 
                cantReach := Set.difference !cantReach seenNodes
            (* Stop if can't reach the end state *)
            let exclude = (fun node -> node <> v && not (canRepeat node) && Set.contains node.Node.Loc seenLocations)
            let reachable = srcWithout cg v exclude Down
            let seenUnmarked = not (Set.isEmpty (Set.intersect !cantReach seenNodes))
            let canReachUnmarked = Set.exists (fun v -> Set.contains v !cantReach) reachable
            if seenUnmarked || canReachUnmarked then
                let canReachDst = Set.contains dst reachable
                if canReachDst then
                    for u in f cg v do
                        let seenNodeBefore = Set.contains u seenNodes
                        let seenLocBefore = Set.contains u.Node.Loc seenLocations
                        let shouldContinue = not seenLocBefore || (canRepeat u && not seenNodeBefore)
                        if shouldContinue then
                            search u (Set.add u.Node.Loc seenLocations) (Set.add u seenNodes)
        search src Set.empty (Set.singleton cg.Start)
        Set.difference allNodes !cantReach

    let alongSimplePathSrcDst cg src dst direction = 
        let canRepeat v = (v.Node.Typ = Topology.Unknown)
        alongSimplePathSrcDstRepeat cg src dst canRepeat direction

    /// Checks if n1 in graph cg1 simulates n2 in cg2
    let supersetPaths (idx: int) (cg1, n1) (cg2, n2) : bool =
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
            logInfo3 (idx, sprintf "Simulate: %s -- %s" (n1.ToString()) (n2.ToString())) 
            let neighbors1 = neighbors cg1 n1 |> Seq.filter isRealNode |> Set.ofSeq
            let neighbors2 = neighbors cg2 n2 |> Seq.filter isRealNode |> Set.ofSeq
            let nchars1 = Set.map (fun v -> v.Node.Loc) neighbors1
            let nchars2 = Set.map (fun v -> v.Node.Loc) neighbors2
            if not (Set.isSuperset nchars1 nchars2) then
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
            match n with 
            | 0 -> true
            | _ ->
                if Map.isEmpty bisim then true else
                if not (remainsSuperset bisim) then false else 
                let b = Map.fold updateBisim (Some Map.empty) bisim
                match b with 
                | None -> false
                | Some b -> iter (n-1) b

        if n1.Node.Loc <> n2.Node.Loc then false 
        else
            let bisim = Map.add n1 (Set.singleton n2) Map.empty
            let steps = max cg1.Graph.VertexCount cg2.Graph.VertexCount 
            iter steps bisim


module Minimize =

    type DominationSet = Dictionary<CgState, Set<CgState>>


    type DominationTree = Dictionary<CgState, CgState option>

    let dominators (dom: DominationTree) x = 
        let mutable ds = Set.add x Set.empty
        if Option.isNone dom.[x] then 
            ds
        else
            let mutable runner = x
            while runner <> Option.get dom.[runner] do 
                ds <- Set.add runner ds
                runner <- Option.get dom.[runner]
            ds

    let inter (po: Dictionary<CgState,int>) (dom: DominationTree) b1 b2 =
        let mutable finger1 = b1
        let mutable finger2 = b2
        while (po.[finger1] <> po.[finger2]) do 
            while (po.[finger1] > po.[finger2]) do 
                finger1 <- Option.get dom.[finger1]
            while (po.[finger2] > po.[finger1]) do
                finger2 <- Option.get dom.[finger2]
        finger1

    let dominators' (cg: T) root direction : DominationSet = 
        let f = if direction = Up then neighbors else neighborsIn
        let dom = Dictionary()
        let postorder = 
            Reachable.dfs cg root direction
            |> Seq.mapi (fun i n -> (n,i))
        let postorderMap = Dictionary()
        Seq.iter (fun (n,i) -> postorderMap.[n] <- i) postorder
        let allNodes = cg.Graph.Vertices |> Set.ofSeq
        for b in allNodes do 
            dom.[b] <- None
        dom.[root] <- Some root
        let mutable changed = true
        while changed do 
            changed <- false 
            for b, i in postorder do
                if b <> root then 
                    let preds = f cg b
                    let mutable newIDom = Seq.find (fun x -> postorderMap.[x] < i) preds
                    for p in preds do
                        if p <> newIDom then 
                            if dom.[p] <> None then 
                                newIDom <- inter postorderMap dom p newIDom
                    let x = dom.[b]
                    if Option.isNone x || Option.get x <> newIDom then
                        dom.[b] <- Some newIDom
                        changed <- true
        let res = Dictionary()
        for v in cg.Graph.Vertices do 
            res.[v] <- dominators dom v
            //printfn "  dominators for %s are: %s" (string v) (string (Set.map string (dominators dom v)))
        res

    (* let dominators (cg: T) root direction : DominationSet =
        let f = if direction = Up then neighbors else neighborsIn
        let dom = Dictionary()
        let nodes = Reachable.dfs cg root direction
        let allNodes = cg.Graph.Vertices |> Set.ofSeq
        for n in allNodes do 
            dom.[n] <- allNodes
        let mutable changed = true 
        while changed do 
            changed <- false 
            for n in nodes do
                let preds = seq {for p in (f cg n) do yield dom.[p]}
                let interAll = if Seq.isEmpty preds then Set.empty else Set.intersectMany preds
                let newSet = Set.union (Set.singleton n) interAll
                if (newSet <> dom.[n]) then 
                    dom.[n] <- newSet
                    changed <- true
        dom *)

    let removeDominated (cg: T) = 
        let dom = dominators' cg cg.Start Down
        let domRev = dominators' cg cg.End Up
        // Remove dominated edges
        cg.Graph.RemoveEdgeIf (fun (e: TaggedEdge<CgState,unit>) -> 
            let ies = cg.Graph.OutEdges e.Target
            match Seq.tryFind (fun (ie: TaggedEdge<CgState,unit>) -> ie.Target = e.Source) ies with 
            | None -> false 
            | Some ie ->
                assert (ie.Source = e.Target)
                assert (ie.Target = e.Source)
                (Set.contains e.Target (dom.[e.Source]) || Set.contains e.Source (domRev.[e.Target])) &&
                not (e.Target = e.Source) ) |> ignore
        // Remove nodes dominated by a similar location
        cg.Graph.RemoveVertexIf (fun v -> 
            Set.exists (fun u -> u <> v && u.Node.Loc = v.Node.Loc) dom.[v]) |> ignore
        // Remove edges where the outgoing is dominated by a similar location
        cg.Graph.RemoveEdgeIf (fun e ->
            let x = e.Source
            let y = e.Target
            Set.exists (fun x' -> x'.Node.Loc = x.Node.Loc && x' <> x) domRev.[y]) |> ignore

    let removeDeadEdgesHeuristic (cg: T) =
        cg.Graph.RemoveEdgeIf (fun (e: TaggedEdge<CgState,unit>) -> 
            let x = e.Target
            not (Reachable.srcDstWithout cg x cg.End (fun v -> v <> x && v.Node.Loc = e.Source.Node.Loc) Down)
        ) |> ignore

    let removeNodesThatCantReachEnd (cg: T) = 
        let canReach = Reachable.src cg cg.End Up
        cg.Graph.RemoveVertexIf(fun v -> 
            Topology.isTopoNode v.Node && not (Set.contains v canReach)
        ) |> ignore
        

    let removeNodesThatStartCantReach (cg: T) = 
        let canReach = Reachable.src cg cg.Start Down
        cg.Graph.RemoveVertexIf(fun v -> 
            Topology.isTopoNode v.Node && not (Set.contains v canReach)
        ) |> ignore

    (* let removeNodesNotReachableOnSimplePath (cg: T) =
        let canReach = Reachable.simplePathSrc cg cg.Start Down
        cg.Graph.RemoveVertexIf (fun v -> Topology.isTopoNode v.Node && not (Set.contains v canReach)) |> ignore *)

    (* let removeNodesNotOnAnySimplePathToEnd (cg: T) = 
        let canReach = Reachable.alongSimplePathSrcDst cg cg.Start cg.End Down
        cg.Graph.RemoveVertexIf (fun v -> Topology.isTopoNode v.Node && not (Set.contains v canReach)) |> ignore *)

    let delMissingSuffixPaths cg = 
        let starting = neighbors cg cg.Start |> Seq.filter isRealNode |> Set.ofSeq
        cg.Graph.RemoveVertexIf (fun v -> 
            v.Node.Typ = Topology.InsideOriginates && 
            v.Accept.IsEmpty && 
            not (Set.contains v starting)
        ) |> ignore

    let compressRepeatedUnknowns (cg: T) = 
        let components = ref (Dictionary(HashIdentity.Structural) :> IDictionary<CgState,int>)
        ignore (cg.Graph.StronglyConnectedComponents(components))
        let mutable sccs = Map.empty 
        for kv in !components do
            let state = kv.Key
            let comp = kv.Value
            if Topology.isOutside state.Node then 
                let existing = Common.Map.getOrDefault comp Set.empty sccs
                sccs <- Map.add comp (Set.add state existing) sccs
        let allOutTopo = 
            cg.Topo.Vertices
            |> Seq.filter Topology.isOutside
            |> Set.ofSeq
        let allOutTopoLocs = Set.map (fun (v: Topology.State) -> v.Loc) allOutTopo
        let sccs = List.map snd (Map.toList sccs)
        for scc in sccs do 
            let locs = Set.map (fun v -> v.Node.Loc) scc
            if (allOutTopo.Count = scc.Count) && 
               (locs = allOutTopoLocs) && 
               (Set.exists (isRepeatedOut cg) scc) then
                let outStar = Set.filter (isRepeatedOut cg) scc |> Set.minElement
                cg.Graph.RemoveEdgeIf (fun e -> e.Source = outStar && isRealNode e.Target && e.Target <> e.Source) |> ignore

    let removeRedundantExternalNodes (cg: T) =
        let toDelNodes = HashSet(HashIdentity.Structural)
        let outside = 
            cg.Graph.Vertices 
            |> Seq.filter (fun v -> Topology.isOutside v.Node)
            |> Set.ofSeq
        for a in outside do 
            let ans = neighbors cg a |> Set.ofSeq
            if ans.Count = 1 then 
                let b = ans.MinimumElement
                if a <> b && isRepeatedOut cg b then 
                    let ians = neighborsIn cg a |> Set.ofSeq 
                    let ibns = neighborsIn cg b |> Set.ofSeq
                    if Set.isSuperset ibns ians then
                        ignore (toDelNodes.Add a)
        for b in cg.Graph.Vertices do 
            if isRepeatedOut cg b then
                let bns = neighbors cg b |> Set.ofSeq
                for a in bns do
                    if a <> b && outside.Contains a then
                        let count = neighborsIn cg a |> Seq.length
                        if count = 1 then
                            let ans = neighbors cg a |> Set.ofSeq
                            if Set.isSuperset bns ans then 
                                ignore (toDelNodes.Add a)
        cg.Graph.RemoveVertexIf (fun v -> toDelNodes.Contains v) |> ignore
           
    let minimize (idx: int) (cg: T) =
        logInfo1(idx, sprintf "Node count: %d" cg.Graph.VertexCount)
        // count vertices + edges to detect change
        let inline count cg = 
            cg.Graph.VertexCount + cg.Graph.EdgeCount
        // repeatedly apply reductions until no change
        let inline prune () = 
            removeNodesThatCantReachEnd cg
            logInfo1(idx, sprintf "Node count (cant reach end): %d" cg.Graph.VertexCount)
            removeDominated cg
            logInfo1(idx, sprintf "Node count (remove dominated): %d" cg.Graph.VertexCount)
            removeNodesThatStartCantReach cg
            logInfo1(idx, sprintf "Node count (start cant reach): %d" cg.Graph.VertexCount)
            // removeDeadEdgesHeuristic cg
            // logInfo1(idx, sprintf "Node count (edge heuristic): %d" cg.Graph.VertexCount)
            removeRedundantExternalNodes cg
            logInfo1(idx, sprintf "Node count (redundant external nodes): %d" cg.Graph.VertexCount)
            compressRepeatedUnknowns cg
            logInfo1(idx, sprintf "Node count (compress out*): %d" cg.Graph.VertexCount)
            (* removeNodesNotOnAnySimplePathToEnd cg*)
        let mutable sum = count cg
        prune() 
        while count cg <> sum do
            sum <- count cg
            prune ()
        logInfo1(idx, sprintf "Node count - after O3: %d" cg.Graph.VertexCount)


module Consistency = 

    exception ConsistencyException of CgState * CgState

    type CounterExample =  CgState * CgState
    type Preferences = seq<CgState>
    type Ordering = Map<string, Preferences>
    type Constraints = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>

    let simulate idx cg restrict (x,y) (i,j) =
        let restrict_i = copyGraph (Map.find i restrict)
        let restrict_j = copyGraph (Map.find j restrict)
        restrict_i.Graph.RemoveVertexIf (fun v -> v.Node.Loc = x.Node.Loc && v <> x) |> ignore
        restrict_j.Graph.RemoveVertexIf (fun v -> v.Node.Loc = y.Node.Loc && v <> y) |> ignore
        if not (restrict_i.Graph.ContainsVertex x) then false
        else if not (restrict_j.Graph.ContainsVertex y) then true
        else
            // Remove nodes that appear 'above' for the more preferred, to avoid considering simple paths
            // cheaper approximation of simple paths - can do better at cost of speed
            let exclude = (fun v -> v.Node.Loc = x.Node.Loc && v <> x)
            let reach = Reachable.srcWithout restrict_i x exclude Up |> Set.map (fun v -> v.Node.Loc)
            restrict_i.Graph.RemoveVertexIf (fun v -> v <> x && Set.contains v.Node.Loc reach) |> ignore
            // Check if the more preferred simulates the less preferred
            Reachable.supersetPaths idx (restrict_i, x) (restrict_j, y) 

    let isPreferred f cg restrict (x,y) (reachX, reachY) =
        let subsumes i j =
            f cg restrict (x,y) (i,j)
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
    
    let getReachabilityMap (cg:T) =
        let prefs = 
            cg.Graph.Vertices 
            |> Seq.map (fun v -> v.Accept)
            |> Seq.fold Set.union Set.empty
        let getNodesWithPref acc i = 
            let copy = copyGraph cg
            copy.Graph.RemoveEdgeIf (fun e -> 
                e.Target = copy.End && not (e.Source.Accept.Contains i)) |> ignore
            let reach = Reachable.src copy copy.End Up
            Set.fold (fun acc v ->
                let existing = Common.Map.getOrDefault v Set.empty acc 
                let updated = Map.add v (Set.add i existing) acc
                updated) acc reach
        Set.fold getNodesWithPref Map.empty prefs    

    let addPrefConstraints idx f cg (g: Constraints) r nodes reachMap =
        let mutable edges = Set.empty
        for x in nodes do
            for y in nodes do
                let reachX = Map.find x reachMap
                let reachY = Map.find y reachMap
                if x <> y && (isPreferred f cg r (x,y) (reachX,reachY)) then
                    logInfo1 (idx, sprintf "%s is preferred to %s" (x.ToString()) (y.ToString()))
                    edges <- Set.add (x,y) edges
                    g.AddEdge (TaggedEdge(x, y, ())) |> ignore
                else if x <> y then
                    logInfo1 (idx, sprintf "%s is NOT preferred to %s" (x.ToString()) (y.ToString()))
        g, edges

    let encodeConstraints idx f (cg, reachMap) r nodes =
        let g = BidirectionalGraph<CgState ,TaggedEdge<CgState,unit>>()
        for n in nodes do 
            g.AddVertex n |> ignore
        addPrefConstraints idx f cg g r nodes reachMap

    let findPrefAssignment idx f r (cg, reachMap) nodes = 
        let g, edges = encodeConstraints idx f (cg, reachMap) r nodes
        getOrdering g edges

    let addForLabel idx ain f r (cg, reachMap) map l =
        let ain = Set.map (fun (v: Topology.State) -> v.Loc) ain
        if ain.Contains l then
            if not (Map.containsKey l map) then 
                let nodes = Seq.filter (fun v -> v.Node.Loc = l) cg.Graph.Vertices
                Map.add l (findPrefAssignment idx f r (cg, reachMap) nodes) map
            else map
        else Map.add l Seq.empty map

    let restrictedGraphs cg prefs =
        let aux acc i =
            let r = restrict cg i 
            Minimize.removeNodesThatCantReachEnd r
            (* don't consider external ASes. Note: don't remove nodes after this *)
            r.Graph.RemoveEdgeIf (fun e -> Topology.isOutside e.Source.Node) |> ignore
            Map.add i r acc
        Set.fold aux Map.empty prefs

    let findOrdering idx f cg outName : Result<Ordering, CounterExample> =
        let prefs = preferences cg 
        let rs = restrictedGraphs cg prefs
        let reachMap = getReachabilityMap cg
        let (ain, _) = Topology.alphabet cg.Topo
        debug2 (fun () -> Map.iter (fun i g -> generatePNG g (outName + "-min-restricted" + string i)) rs)
        let labels = 
            cg.Graph.Vertices
            |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
            |> Seq.map (fun v -> v.Node.Loc)
            |> Set.ofSeq 
        try Ok(Set.fold (addForLabel idx ain f rs (cg, reachMap)) Map.empty labels)
        with ConsistencyException(x,y) ->
            Err((x,y) )

    let findOrderingConservative (idx: int) = findOrdering idx (simulate idx)


module ToRegex =

    let constructRegex (cg: T) (state: CgState) : Regex.T =
        (* Store regex transitions in a separate map *)
        let reMap = ref Map.empty
        let get v = Common.Map.getOrDefault v Regex.empty !reMap
        let add k v = reMap := Map.add k v !reMap

        (* Simplify graph to only contain relevant nodes *)
        let reachable = Reachable.src cg state Down
        cg.Graph.RemoveVertexIf (fun v -> not (reachable.Contains v) && Topology.isTopoNode v.Node) |> ignore
        cg.Graph.AddEdge (TaggedEdge(cg.End, state, ())) |> ignore

        (* Populate the regex transition map *)
        add (cg.End, state) Regex.epsilon
        for e in cg.Graph.Edges do
            if e.Source <> cg.End then
                add (e.Source, e.Target) (Regex.loc e.Source.Node.Loc)
        
        (* we will remove all none start/end nodes one by one *)
        let queue = Queue()
        for v in cg.Graph.Vertices do
            if isRealNode v then
                queue.Enqueue v
        
        (* repeatedly pick a next node and remove it, updating path regexes *)
        while queue.Count > 0 do 
            let q = queue.Dequeue()
            for q1 in cg.Graph.Vertices do 
                for q2 in cg.Graph.Vertices do
                    if q1 <> q && q2 <> q then
                        let x = get (q1,q2)
                        let y1 = get (q1,q)
                        let y2 = get (q,q)
                        let y3 = get (q,q2)
                        let re = Regex.union x (Regex.concatAll [y1; Regex.star y2; y3])
                        reMap := Map.add (q1,q2) re !reMap
            cg.Graph.RemoveVertex q |> ignore
        
        Map.find (cg.End, cg.Start) !reMap


module Failure =

    type FailType =
        | NodeFailure of Topology.State
        | LinkFailure of TaggedEdge<Topology.State,unit>

        override this.ToString() = 
            match this with 
            | NodeFailure n -> "Node(" + n.Loc + ")"
            | LinkFailure e -> "Link(" + e.Source.Loc + "," + e.Target.Loc + ")"
  
    let allFailures n (topo: Topology.T) : seq<FailType list> =
        let fvs = topo.Vertices |> Seq.filter Topology.isInside |> Seq.map NodeFailure
        let fes =
            topo.Edges
            |> Seq.filter (fun e -> Topology.isInside e.Source || Topology.isInside e.Target) 
            |> Seq.map LinkFailure 
        Seq.append fes fvs 
        |> Seq.toList
        |> Common.List.combinations n

    let failedGraph (cg: T) (failures: FailType list) : T =
        let failed = copyGraph cg
        let rec aux acc fs =
            let (vs,es) = acc 
            match fs with
            | [] -> acc
            | (NodeFailure s)::tl ->
                aux (s.Loc::vs, es) tl
            | (LinkFailure s)::tl ->
                aux (vs, (s.Source.Loc, s.Target.Loc)::(s.Target.Loc, s.Source.Loc)::es) tl
        let (failedNodes, failedEdges) = aux ([],[]) failures
        failed.Graph.RemoveVertexIf (fun v -> 
            List.exists ((=) v.Node.Loc) failedNodes) |> ignore
        failed.Graph.RemoveEdgeIf (fun e -> 
            List.exists ((=) (e.Source.Node.Loc, e.Target.Node.Loc)) failedEdges) |> ignore
        failed