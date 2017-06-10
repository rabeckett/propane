module CGraph

open QuickGraph
open QuickGraph.Algorithms
open System
open System.Collections.Generic
open System.Diagnostics
open Util.Debug
open Util.Error
open Util.Format

/// Individual node in the Product Graph. Contains:
///
/// .Id     a unique id to speed up hashing/comparisons in graph algorithms
/// .State  a unique representation of all automata states (q1,...,qk)
/// .Accept a 16 bit integer representing the lowest (best) preference
///         the value System.Int16.MaxValue represents a non-accepting node
/// .Node   the underlying topology node, including name + internal/external
[<CustomEquality; CustomComparison>]
type CgState = 
   { Id : int
     State : int
     Accept : int16
     Node : Topology.Node }
   override this.ToString() = "(" + string this.State + ", " + this.Node.Loc + ")"
   
   override x.Equals(other) = 
      match other with
      | :? CgState as y -> (x.Id = y.Id)
      | _ -> false
   
   override x.GetHashCode() = x.Id
   interface System.IComparable with
      member x.CompareTo other = 
         match other with
         | :? CgState as y -> x.Id - y.Id
         | _ -> failwith "cannot compare values of different types"

/// An intermediate representation of Product graph nodes.
/// This is a bit of a temporary hack. It simplifies product graph construction
/// since the default hashing and comparison operators can be used during 
/// construction, before we have assigned a unique id for each state and node.
/// We throw this away after construction since it is less space inefficient.
type CgStateTmp = 
   { TStates : int array
     TNode : Topology.Node
     TAccept : int16 }

/// Type of the Product Graph. Contains:
/// 
/// .Start  A unique start node. Anything routers connected 
///         to the start node can originate traffic
/// .End    A unique end node, which is connected to all accepting
///         nodes. This is included to simplify some algorithms
/// .Topo   The underlying topology object
type T = 
   { Start : CgState
     End : CgState
     Graph : BidirectionalGraph<CgState, Edge<CgState>>
     Topo : Topology.T }

/// Direction of search. We often need to search in the reverse graph,
/// yet do not want to make a copy of the graph every time
type Direction = 
   | Up
   | Down

/// We represent the non-accepting state as the maximum
/// 16 bit integer value. This makes comparisons in the
/// failure analysis easy.
let NO_ACCEPT = System.Int16.MaxValue

let MAX_PREF = System.Int16.MaxValue

let copyGraph (cg : T) : T = 
   let newCG = QuickGraph.BidirectionalGraph()
   for v in cg.Graph.Vertices do
      newCG.AddVertex v |> ignore
   for e in cg.Graph.Edges do
      newCG.AddEdge e |> ignore
   { Start = cg.Start
     Graph = newCG
     End = cg.End
     Topo = cg.Topo }

let copyReverseGraph (cg : T) : T = 
   let newCG = QuickGraph.BidirectionalGraph()
   for v in cg.Graph.Vertices do
      newCG.AddVertex v |> ignore
   for e in cg.Graph.Edges do
      let e' = Edge(e.Target, e.Source)
      newCG.AddEdge e' |> ignore
   { Start = cg.Start
     Graph = newCG
     End = cg.End
     Topo = cg.Topo }

/// Convert the space-inefficient product graph that keeps an array
/// of automata states in each node, into a space-efficient product
/// graph that represents each unique tuple with a single integer
let index ((graph, topo, startNode, endNode) : BidirectionalGraph<_, _> * _ * _ * _) = 
   let newCG = QuickGraph.BidirectionalGraph()
   let reindex = Util.Reindexer(HashIdentity.Structural)
   
   let nstart = 
      { Id = 0
        Node = startNode.TNode
        State = (reindex.Index startNode.TStates)
        Accept = startNode.TAccept }
   
   let nend = 
      { Id = 1
        Node = endNode.TNode
        State = (reindex.Index endNode.TStates)
        Accept = endNode.TAccept }
   
   ignore (newCG.AddVertex nstart)
   ignore (newCG.AddVertex nend)
   let mutable i = 2
   let idxMap = Dictionary(HashIdentity.Structural)
   idxMap.[(nstart.Node.Loc, nstart.State)] <- nstart
   idxMap.[(nend.Node.Loc, nend.State)] <- nend
   for v in graph.Vertices do
      if Topology.isTopoNode v.TNode then 
         let newv = 
            { Id = i
              Node = v.TNode
              State = (reindex.Index v.TStates)
              Accept = v.TAccept }
         i <- i + 1
         idxMap.Add((v.TNode.Loc, reindex.Index v.TStates), newv)
         newCG.AddVertex newv |> ignore
   for e in graph.Edges do
      let v = e.Source
      let u = e.Target
      let x = idxMap.[(v.TNode.Loc, reindex.Index v.TStates)]
      let y = idxMap.[(u.TNode.Loc, reindex.Index u.TStates)]
      newCG.AddEdge(Edge(x, y)) |> ignore
   { Start = nstart
     Graph = newCG
     End = nend
     Topo = topo }

let getTransitions autos = 
   let aux (auto : Regex.Automaton) = 
      let trans = Dictionary(HashIdentity.Structural)
      for kv in auto.trans do
         let (q1, S) = kv.Key
         let q2 = kv.Value
         for s in S do
            trans.[(q1, s)] <- q2
      trans
   Array.map aux autos

let getGarbageStates (auto : Regex.Automaton) = 
   let inline aux (kv : KeyValuePair<_, _>) = 
      let k = kv.Key
      let v = kv.Value
      let c = Set.count v
      if c = 1 && Set.minElement v = k then Some k
      else None
   
   let selfLoops = 
      Map.fold (fun acc (x, _) y -> 
         let existing = Util.Map.getOrDefault x Set.empty acc
         Map.add x (Set.add y existing) acc) Map.empty auto.trans
      |> Seq.choose aux
      |> Set.ofSeq
   
   Set.difference selfLoops auto.F

let inline uniqueNeighbors canOriginate topo (t : Topology.Node) = 
   (if t.Typ = Topology.Start then canOriginate
    else Topology.neighbors topo t)
   |> Set.ofSeq

/// Build the product graph from a topology and collection of 
/// (ordered) regular expressions. Performs a multi-way intersection 
/// of each automata with the topology. As an optimization, we avoid 
/// constructing parts of the product graph prematurely when we are 
/// guaranteed it will never lead to an accepting state.
let buildFromAutomata (topo : Topology.T) (autos : Regex.Automaton array) : T = 
   if autos.Length >= int MAX_PREF then 
      error (sprintf "Propane does not currently support more than %d preferences" MAX_PREF)
   if not (Topology.isWellFormed topo) then 
      error (sprintf "Invalid topology. Topology must be connected.")
   let unqTopo = Set.ofSeq (Topology.vertices topo)
   let canOrigin = Seq.filter Topology.canOriginateTraffic unqTopo
   let transitions = getTransitions autos
   let garbage = Array.map getGarbageStates autos
   let graph = BidirectionalGraph<CgStateTmp, Edge<CgStateTmp>>()
   let starting = Array.map (fun (x : Regex.Automaton) -> x.q0) autos
   
   let newStart = 
      { TStates = starting
        TAccept = NO_ACCEPT
        TNode = Topology.Node("start", Topology.Start) }
   ignore (graph.AddVertex newStart)
   let marked = HashSet(HashIdentity.Structural)
   let todo = Queue()
   todo.Enqueue newStart
   while todo.Count > 0 do
      let currState = todo.Dequeue()
      let t = currState.TNode
      let adj = uniqueNeighbors canOrigin topo t
      
      let adj = 
         match t.Typ with
         | Topology.Unknown _ -> Set.add t adj
         | _ -> adj
      //if t.Typ = Topology.Unknown then Set.add t adj
      //else adj
      for c in adj do
         let dead = ref true
         
         let nextInfo = 
            Array.init autos.Length (fun i -> 
               let g, v = autos.[i], currState.TStates.[i]
               let newState = transitions.[i].[(v, c.Loc)]
               if not (garbage.[i].Contains newState) then dead := false
               let accept = 
                  if (Topology.canOriginateTraffic c) && (Set.contains newState g.F) then 
                     int16 (i + 1)
                  else NO_ACCEPT
               newState, accept)
         
         let nextStates, nextAccept = Array.unzip nextInfo
         let accept = Array.fold min NO_ACCEPT nextAccept
         
         let state = 
            { TStates = nextStates
              TAccept = accept
              TNode = c }
         if not !dead then 
            if not (marked.Contains state) then 
               ignore (marked.Add state)
               ignore (graph.AddVertex state)
               todo.Enqueue state
            let edge = Edge(currState, state)
            ignore (graph.AddEdge edge)
   let newEnd = 
      { TStates = [||]
        TAccept = NO_ACCEPT
        TNode = Topology.Node("end", Topology.End) }
   graph.AddVertex newEnd |> ignore
   for v in graph.Vertices do
      if v.TAccept <> NO_ACCEPT then 
         let e = Edge(v, newEnd)
         ignore (graph.AddEdge(e))
   index (graph, topo, newStart, newEnd)

let loc x = x.Node.Loc
let shadows x y = (loc x = loc y) && (x <> y)

let preferences (cg : T) : Set<int16> = 
   let mutable all = Set.empty
   for v in cg.Graph.Vertices do
      if v.Accept <> NO_ACCEPT then all <- Set.add v.Accept all
   all

let acceptingStates (cg : T) : Set<CgState> = 
   cg.Graph.Vertices
   |> Seq.filter (fun (v : CgState) -> v.Accept <> NO_ACCEPT)
   |> Set.ofSeq

let acceptingLocations (cg : T) : Set<string> = acceptingStates cg |> Set.map loc
let isRealNode (state : CgState) = Topology.isTopoNode state.Node

let neighbors (cg : T) (state : CgState) = 
   seq { 
      for e in cg.Graph.OutEdges state do
         yield e.Target
   }

let neighborsIn (cg : T) (state : CgState) = 
   seq { 
      for e in cg.Graph.InEdges state do
         yield e.Source
   }

let repeatedOuts (cg : T) = 
   seq { 
      for v in cg.Graph.Vertices do
         match v.Node.Typ with
         | Topology.Unknown vs -> 
            if (Set.isEmpty vs && Seq.contains v (neighbors cg v)) then yield v
         | _ -> ()
   }
   |> Set.ofSeq

(* let isRepeatedOut (cg : T) (state : CgState) = 
   match state.Node.Typ, Seq.contains state (neighbors cg state) with
   | Topology.Unknown vs, true -> Set.isEmpty vs
   | _, _ -> false *)

//(state.Node.Typ = Topology.Unknown) && (Seq.contains state (neighbors cg state))
let isInside x = Topology.isInside x.Node
let isOutside x = Topology.isOutside x.Node
let isEmpty (cg : T) = cg.Graph.VertexCount = 2

let toDot (cg : T) (pi : Ast.PolInfo) : string = 
   let onFormatEdge (e : Graphviz.FormatEdgeEventArgs<CgState, Edge<CgState>>) = ()
   
   let onFormatVertex (v : Graphviz.FormatVertexEventArgs<CgState>) = 
      let state = string v.Vertex.State
      
      let location = 
         let l = Topology.router (loc v.Vertex) pi.Ast.TopoInfo
         match v.Vertex.Node.Typ with
         | Topology.Unknown excls -> 
            let s = 
               if Set.isEmpty excls then "{}"
               else sprintf "{%s}" (Util.Set.joinBy "," excls)
            sprintf "out-%s" s
         | _ -> l
      match v.Vertex.Node.Typ with
      | Topology.Start -> v.VertexFormatter.Label <- "Start"
      | Topology.End -> v.VertexFormatter.Label <- "End"
      | _ -> 
         if v.Vertex.Accept = NO_ACCEPT then 
            let label = sprintf "%s, %s" state location
            v.VertexFormatter.Label <- label
         else 
            let label = sprintf "%s, %s, Rank=%d" state location v.Vertex.Accept
            v.VertexFormatter.Label <- label
            v.VertexFormatter.Shape <- Graphviz.Dot.GraphvizVertexShape.DoubleCircle
            v.VertexFormatter.Style <- Graphviz.Dot.GraphvizVertexStyle.Filled
            v.VertexFormatter.FillColor <- Graphviz.Dot.GraphvizColor.LightYellow
   
   let graphviz = Graphviz.GraphvizAlgorithm<CgState, Edge<CgState>>(cg.Graph)
   graphviz.FormatEdge.Add(onFormatEdge)
   graphviz.FormatVertex.Add(onFormatVertex)
   graphviz.Generate()

/// To help with debugging, we generate a graphical representation
/// of the product graph in the graphviz dot format.
/// A png can be generated if the 'dot' command line tool
/// is installed and in the system path.
let generatePNG (cg : T) pi (file : string) : unit = 
   System.IO.File.WriteAllText(file + ".dot", toDot cg pi)
   let p = new Process()
   p.StartInfo.FileName <- "dot"
   p.StartInfo.UseShellExecute <- false
   p.StartInfo.Arguments <- "-Tpng " + file + ".dot -o " + file + ".png"
   p.StartInfo.CreateNoWindow <- true
   p.Start() |> ignore
   p.WaitForExit()

/// Helper functions for common reachability queries.
/// We avoid using the default QuickGraph algorithms since 
/// (1)  We want to abstract over direction
/// (2)  We want to take advantage of the unique node id
module Reachable = 
   let postOrder (cg : T) (source : CgState) direction : List<CgState> = 
      let f = 
         if direction = Up then neighborsIn
         else neighbors
      
      let marked = HashSet()
      let ret = ResizeArray()
      let s = Stack()
      s.Push(source)
      while s.Count > 0 do
         let v = s.Pop()
         if not (marked.Contains v) then 
            ignore (marked.Add v)
            ret.Add(v)
            for w in f cg v do
               s.Push(w)
      ret
   
   let dfs (cg : T) (source : CgState) direction : HashSet<CgState> = 
      let f = 
         if direction = Up then neighborsIn
         else neighbors
      
      let marked = HashSet()
      let s = Stack()
      s.Push(source)
      while s.Count > 0 do
         let v = s.Pop()
         if not (marked.Contains v) then 
            ignore (marked.Add v)
            for w in f cg v do
               s.Push(w)
      marked
   
   let path (cg : T) (source : CgState) (target : CgState) : List<Edge<CgState>> option = 
      let marked = HashSet()
      let edgeTo = Dictionary()
      let s = Queue()
      ignore (marked.Add(source))
      s.Enqueue(source)
      let mutable search = true
      while search && s.Count > 0 do
         let v = s.Dequeue()
         if v = target then search <- false
         for e in cg.Graph.OutEdges v do
            let w = e.Target
            if not (marked.Contains w) then 
               ignore (marked.Add w)
               edgeTo.[w] <- e
               s.Enqueue(w)
      if search then None
      else 
         let path = List()
         let mutable x = target
         while x <> source do
            let e = edgeTo.[x]
            path.Add(e)
            x <- e.Source
         Some(path)
   
   let srcAccepting (cg : T) (source : CgState) direction : Set<int16> = 
      let f = 
         if direction = Up then neighborsIn
         else neighbors
      let mutable ret = Set.empty
      let marked = HashSet()
      let s = Stack()
      s.Push(source)
      while s.Count > 0 do
         let v = s.Pop()
         if v.Accept <> NO_ACCEPT then ret <- Set.add v.Accept ret
         if not (marked.Contains v) then 
            ignore (marked.Add v)
            for w in f cg v do
               s.Push(w)
      ret

let createDag (cg : T) : T = 
   let copy = copyGraph cg
   let toDelete = HashSet()
   let length = Dictionary()
   let marked = HashSet()
   let s = Queue()
   // initialize path length to 0
   for v in cg.Graph.Vertices do
      length.Add(v, Int32.MaxValue) |> ignore
   // walk over nodes are remove edges
   s.Enqueue((copy.Start,0))
   while s.Count > 0 do
      let (v,n) = s.Dequeue()
      if not (marked.Contains v) then
         length.[v] <- n
         marked.Add v |> ignore
         for e in copy.Graph.OutEdges v do 
            s.Enqueue((e.Target,n+1))

   copy.Graph.RemoveEdgeIf (fun e -> length.[e.Source] >= length.[e.Target] && e.Target <> copy.End) |> ignore
   copy

/// Implementation of graph dominators as described in:
///
/// A Simple, Fast Dominance Algorithm ~ https://www.cs.rice.edu/~keith/EMBED/dom.pdf
///
/// Uses an O(n^2) algorithm for computing dominators of every node in the
/// product graph, where n is the number of nodes in the product graph.
/// 
/// However, this runs fast in practice since the sets of dominators are stored
/// compactly in a data structure called a dominator tree.
(* let dominationLocs (cg : T) : Map<CgState, Set<string>> = 
   let allVertices = cg.Graph.Vertices |> Set.ofSeq
   let allLocs = Set.map loc allVertices
   let postorder = Reachable.postOrder cg cg.Start Down
   let mutable acc = Map.add cg.Start Set.empty Map.empty
   for v in postorder do
      if v <> cg.Start then acc <- Map.add v allLocs acc
   let mutable change = true
   while change do
      change <- false
      for v in postorder do
         if v <> cg.Start then 
            let nsIn = neighborsIn cg v |> Set.ofSeq
            let nsLocs = Seq.map (fun k -> Map.find k acc |> Set.add k.Node.Loc) nsIn
            let existing = Map.find v acc
            
            let newLocs = 
               if Seq.isEmpty nsLocs then Set.empty
               else Set.intersectMany nsLocs
            if Set.count newLocs <> Set.count existing then change <- true
            acc <- Map.add v newLocs acc
   acc *)

module Domination = 
   type DomTreeMapping = Dictionary<CgState, CgState option>
   
   [<Struct>]
   type DominationTree(tree : DomTreeMapping) = 
      
      member this.IsDominatedBy(x, y) = 
         match tree.[x] with
         | None -> false
         | Some v -> 
            let mutable runner = x
            let mutable current = v
            let mutable found = false
            while not found && runner <> current do
               if runner = y then found <- true
               runner <- current
               current <- tree.[runner].Value
            found
      
      member this.IsDominatedByFun(x, f) = 
         match tree.[x] with
         | None -> false
         | Some v -> 
            let mutable runner = x
            let mutable current = v
            let mutable found = false
            while not found && runner <> current do
               if f runner then found <- true
               runner <- current
               current <- tree.[runner].Value
            found
      
      member this.TryIsDominatedBy(x, f) = 
         match tree.[x] with
         | None -> None
         | Some v -> 
            let mutable runner = x
            let mutable current = v
            let mutable found = None
            while Option.isNone found && runner <> current do
               if f runner then found <- Some runner
               runner <- current
               current <- tree.[runner].Value
            found
   
   let inter (po : Dictionary<CgState, int>) (dom : DomTreeMapping) b1 b2 = 
      let mutable finger1 = b1
      let mutable finger2 = b2
      let mutable x = po.[finger1]
      let mutable y = po.[finger2]
      while x <> y do
         while x > y do
            finger1 <- Option.get dom.[finger1]
            x <- po.[finger1]
         while y > x do
            finger2 <- Option.get dom.[finger2]
            y <- po.[finger2]
      finger1
   
   let dominators (cg : T) root direction : DominationTree = 
      let adj = 
         if direction = Up then neighbors cg
         else neighborsIn cg
      
      let dom = Dictionary()
      let reach = Reachable.postOrder cg root direction
      let postorder = Seq.mapi (fun i n -> (n, i)) reach
      let postorderMap = Dictionary()
      for (n, i) in postorder do
         postorderMap.[n] <- i
      let inline findBefore i preds = 
         let aux x = 
            let (b, v) = postorderMap.TryGetValue x
            b && (v < i)
         Seq.find aux preds
      for b in cg.Graph.Vertices do
         dom.[b] <- None
      dom.[root] <- Some root
      let mutable changed = true
      while changed do
         changed <- false
         for b, i in postorder do
            if b <> root then 
               let preds = adj b
               let mutable newIDom = findBefore i preds
               for p in preds do
                  if p <> newIDom then 
                     if dom.[p] <> None then newIDom <- inter postorderMap dom p newIDom
               let x = dom.[b]
               if Option.isNone x || x.Value <> newIDom then 
                  dom.[b] <- Some newIDom
                  changed <- true
      DominationTree(dom)

/// We perform multiple minimization passes over the product graph
/// after construction for the following reasons:
/// (1)  Can speed up compilation by reducing the size of the graph
/// (2)  Can make the compiled configs smaller by pruning irrelevant cases
/// (3)  Can improve the failure analysis, by ruling out certain cases
module Minimize = 
   type Edge = 
      struct
         val X : int
         val Y : int
         new(x, y) = 
            { X = x
              Y = y }
      end
   
   let edgeSet (cg : T) = 
      let acc = HashSet()
      for e in cg.Graph.Edges do
         let e = Edge(e.Source.Id, e.Target.Id)
         ignore (acc.Add e)
      acc
   
   /// Remove nodes/edges that are never on any non-loop path.
   /// There are 4 symmetric cases.
   let removeDominated (cg : T) = 
      let routs = repeatedOuts cg
      let dom = Domination.dominators cg cg.Start Down
      let domRev = Domination.dominators cg cg.End Up
      cg.Graph.RemoveVertexIf
         (fun v -> 
         (not (routs.Contains v)) 
         && (dom.IsDominatedByFun(v, shadows v) || domRev.IsDominatedByFun(v, shadows v))) |> ignore
      let edges = edgeSet cg
      cg.Graph.RemoveEdgeIf
         (fun e -> 
         let x = e.Source
         let y = e.Target
         let e = Edge(y.Id, x.Id)
         (edges.Contains e) && (not (routs.Contains x || routs.Contains y)) 
         && (dom.IsDominatedBy(x, y) || domRev.IsDominatedBy(y, x)) && (x <> y))
      |> ignore
      cg.Graph.RemoveEdgeIf
         (fun e -> 
         let x = e.Source
         let y = e.Target
         (not (routs.Contains e.Source || routs.Contains e.Target)) 
         && (domRev.IsDominatedByFun(y, shadows x)))
      |> ignore
   
   /// Combines a node out-{N} with a node N into a new state: out
   let mergeNodes (cg : T) out state (merged, candidates) = 
      let nsOut = neighbors cg out |> Set.ofSeq
      let nsIn = neighborsIn cg out |> Set.ofSeq
      // remove old node
      cg.Graph.RemoveVertex out |> ignore
      // remove all nodes to be merged
      for n in merged do
         cg.Graph.RemoveVertex n |> ignore
      // add new state
      cg.Graph.AddVertex state |> ignore
      // add back edges
      for neigh in nsOut do
         if not (Set.contains neigh merged) then 
            let edge = 
               if neigh = out then Edge<CgState>(state, state)
               else Edge<CgState>(state, neigh)
            cg.Graph.AddEdge edge |> ignore
      for neigh in nsIn do
         if not (Set.contains neigh merged) then 
            if neigh <> out then 
               let edge = Edge<CgState>(neigh, state)
               cg.Graph.AddEdge edge |> ignore
   
   let getCandidates out = 
      match out.Node.Typ with
      | Topology.Unknown vs -> vs
      | _ -> Set.empty
   
   /// Combines nodes out-{X,Y} and Y into out-{X} 
   /// when they share all neighbors and have edges to each other
   let combineAsOut (cg : T) = 
      let outs = Seq.filter (fun v -> Topology.isUnknown v.Node) cg.Graph.Vertices
      let mutable toMerge = Seq.empty
      for out in outs do
         let mutable candidates = getCandidates out
         if candidates.Count > 0 then 
            let nsOut = neighbors cg out |> Set.ofSeq
            let nsIn = neighborsIn cg out |> Set.ofSeq
            for n in nsOut do
               if n.Accept = out.Accept && candidates.Contains(loc n) then 
                  let nsOut' = neighbors cg n |> Set.ofSeq
                  let nsIn' = neighborsIn cg n |> Set.ofSeq
                  if (nsOut' = Set.remove n nsOut) && (nsIn' = Set.remove n nsIn) then 
                     candidates <- Set.remove n.Node.Loc candidates
                     let x = Seq.singleton (out, n)
                     toMerge <- Seq.append x toMerge
      let groups = Seq.groupBy fst toMerge
      for (out, gs) in groups do
         let candidates = getCandidates out
         let mutable merged = Set.empty
         let mutable notMerged = candidates
         for (_, n) in gs do
            merged <- Set.add n merged
            notMerged <- Set.remove n.Node.Loc notMerged
         let node = Topology.Node(out.Node.Loc, Topology.Unknown notMerged)
         
         let newState = 
            { Accept = out.Accept
              Id = out.Id
              State = out.State
              Node = node }
         mergeNodes cg out newState (merged, candidates)
   
   let removeNodesThatCantReachEnd (cg : T) = 
      let canReach = Reachable.dfs cg cg.End Up
      cg.Graph.RemoveVertexIf(fun v -> Topology.isTopoNode v.Node && not (canReach.Contains v)) 
      |> ignore
   
   let removeNodesThatStartCantReach (cg : T) = 
      let canReach = Reachable.dfs cg cg.Start Down
      cg.Graph.RemoveVertexIf(fun v -> Topology.isTopoNode v.Node && not (canReach.Contains v)) 
      |> ignore
   
   let inline allConnected cg outStar scc = 
      Set.forall (fun x -> 
         let nOut = Set.ofSeq (neighbors cg x)
         let nIn = Set.ofSeq (neighborsIn cg x)
         x = outStar || (nOut.Contains outStar && nIn.Contains outStar)) scc
   
   let pickBorders (e : Edge<CgState>) = 
      if isInside e.Source && isOutside e.Target then Some e.Source
      else None
   
   (* let combineExportNeighborsAsOut (ti : Topology.TopoInfo) (cg : T) = 
      let topo = ti.SelectGraphInfo.Graph
      let borders = Seq.choose pickBorders cg.Graph.Edges |> Set.ofSeq
      let outStars = repeatedOuts cg
      let mutable toRemove = []
      for b in borders do
         let nsOut = neighbors cg b |> Seq.filter isOutside
         
         let locs = 
            nsOut
            |> Seq.map (fun v -> v.Node.Loc)
            |> Set.ofSeq
         
         let ranks = nsOut |> Seq.map (fun v -> v.Accept)
         let minRank = Seq.min ranks
         let maxRank = Seq.max ranks
         if Seq.length nsOut > 0 && minRank = maxRank && minRank <> NO_ACCEPT then 
            // check if all same in/out
            let mutable acc = true
            for n in nsOut do
               if Seq.length (neighborsIn cg n) <> 1 then acc <- false
               let outs = 
                  neighbors cg n
                  |> Seq.filter isRealNode
                  |> Set.ofSeq
               if Set.count outs <> 1 then acc <- false
               else 
                  let x = Set.minElement outs
                  if not (outStars.Contains x) || x.Accept <> n.Accept then acc <- false
            if acc then 
               let allTopoPeers = 
                  Topology.neighbors topo b.Node
                  |> Seq.filter Topology.isOutside
                  |> Seq.map (fun x -> x.Loc)
                  |> Set.ofSeq
               if locs = allTopoPeers then 
                  // check if same out neighbors
                  toRemove <- (b, Set.ofSeq nsOut) :: toRemove
      for (v, nsOut) in toRemove do
         let x = Seq.head nsOut
         let x = neighbors cg x |> Seq.head
         printfn "%s points to:  %s" (string v) (string x)
         printfn "neighbors: %A" (Seq.map (fun x -> Topology.router x.Node.Loc ti) nsOut)
         cg.Graph.AddEdge(Edge<CgState>(v, x)) |> ignore
         for y in nsOut do
            cg.Graph.RemoveVertex y |> ignore
      printfn "To remove: %A" (List.length toRemove) *)
   let removeConnectionsToOutStar (cg : T) = 
      let routs = repeatedOuts cg
      cg.Graph.RemoveEdgeIf(fun e -> 
         let x = e.Source
         let y = e.Target
         let realNodes = isRealNode x && isRealNode y
         let eqRanks = x.Accept = y.Accept
         if realNodes && eqRanks && x <> y then 
            if routs.Contains x then x.Accept <> NO_ACCEPT
            else if routs.Contains y then 
               Seq.exists isInside (neighbors cg x) 
               && (Seq.exists ((=) cg.Start) (neighborsIn cg y))
            else false
         else false)
      |> ignore
   
   let removeRedundantExternalNodes (cg : T) = 
      let toDelNodes = HashSet(HashIdentity.Structural)
      let routs = repeatedOuts cg
      for os in routs do
         let nos = Set.ofSeq (neighborsIn cg os)
         for n in Set.remove os nos do
            if cg.Graph.OutDegree n = 1 && isOutside n then 
               let nin = Set.ofSeq (neighborsIn cg n)
               if Set.isSuperset nos nin then ignore (toDelNodes.Add n)
      for os in routs do
         let nos = Set.ofSeq (neighbors cg os)
         for n in Set.remove os nos do
            if cg.Graph.InDegree n = 1 && isOutside n then 
               let nin = Set.ofSeq (neighbors cg n)
               if Set.isSuperset nos nin then ignore (toDelNodes.Add n)
      cg.Graph.RemoveVertexIf(fun v -> toDelNodes.Contains v) |> ignore
   
   let minimize (idx : int) (ti : Topology.TopoInfo) (cg : T) = 
      let settings = Args.getSettings()
      if not settings.Minimize then cg
      else 
         let isConcrete = not settings.IsAbstract
         logInfo (idx, sprintf "Node count: %d" cg.Graph.VertexCount)
         let inline count cg = cg.Graph.VertexCount + cg.Graph.EdgeCount
         
         let inline prune() = 
            removeNodesThatCantReachEnd cg
            logInfo (idx, sprintf "Node count (cant reach end): %d" cg.Graph.VertexCount)
            combineAsOut cg
            logInfo (idx, sprintf "Node count (combine external nodes): %d" cg.Graph.VertexCount)
            removeRedundantExternalNodes cg
            logInfo (idx, sprintf "Node count (redundant external nodes): %d" cg.Graph.VertexCount)
            removeConnectionsToOutStar cg
            logInfo (idx, sprintf "Node count (connections to out*): %d" cg.Graph.VertexCount)
            // combineExportNeighborsAsOut ti cg
            // logInfo (idx, sprintf "Node count (merge export neighbors): %d" cg.Graph.VertexCount)
            if isConcrete then 
               removeDominated cg
               logInfo (idx, sprintf "Node count (remove dominated): %d" cg.Graph.VertexCount)
            removeNodesThatStartCantReach cg
            logInfo (idx, sprintf "Node count (start cant reach): %d" cg.Graph.VertexCount)
         
         let mutable sum = count cg
         prune()
         while count cg <> sum do
            sum <- count cg
            prune()
         logInfo (idx, sprintf "Node count - after O3: %d" cg.Graph.VertexCount)
         cg

/// Run failure analysis on minimized product graph.
/// The goal of the failure analysis is to:
/// 
/// Compute a collection of local preferences for each router
/// such that end-to-end preferences are respected regardless
/// of any combination of failures (node or edge) in the network.
///
/// See the paper for details on how this works.
module Consistency = 
   type Neighbor = string
   
   type FailurePoint = CgState * CgState * CgState list
   
   type Explanation = (Neighbor * Neighbor) option * (FailurePoint * FailurePoint) option
   
   exception SimplePathException of CgState * CgState
   
   exception ConsistencyException of CgState * CgState * Explanation
   
   type CounterExample = CgState * CgState * Explanation
   
   type Preferences = seq<CgState>
   
   type Ordering = Dictionary<string, Preferences>
   
   type Constraints = BidirectionalGraph<CgState, Edge<CgState>>
   
   type CacheEntry = 
      struct
         val X : int
         val Y : int
         new(x, y) = 
            { X = x
              Y = y }
      end
   
   type Node = 
      struct
         val More : CgState
         val Less : CgState
         new(m, l) = 
            { More = m
              Less = l }
      end
   
   type ProtectResult = 
      | Yes of HashSet<Node>
      | No of CgState * CgState * CgState
   
   let coveringExternal x' y' = 
      match x'.Node.Typ with
      | Topology.Unknown excls -> not (excls.Contains y'.Node.Loc)
      | _ -> false
   
   let protect (idx : int) (doms : Domination.DominationTree) cg n1 n2 : ProtectResult = 
      let settings = Args.getSettings()
      assert (loc n1 = loc n2)
      let q = Queue()
      let seen = HashSet()
      let counterEx = ref None
      
      // add nodes if preserves the preference relation
      let inline add x' y' (x, y) = 
         if isInside x && isInside y then 
            let i, j = x'.Accept, y'.Accept
            if i > j && (isInside x') && (isInside y') then counterEx := Some(x, y, y')
            else 
               let n' = Node(x', y')
               if not (seen.Contains n') then 
                  ignore (seen.Add n')
                  q.Enqueue n'
      // add initial node
      add n1 n2 (n1, n2)
      while q.Count > 0 && Option.isNone !counterEx do
         let n = q.Dequeue()
         let x = n.More
         let y = n.Less
         if isInside x && isInside y then 
            let nsx = neighbors cg x |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
            let nsxMap = Seq.fold (fun acc x -> Map.add (loc x) x acc) Map.empty nsx
            let nsy = neighbors cg y |> Seq.filter (fun v -> Topology.isTopoNode v.Node)
            for y' in nsy do
               match Map.tryFind (loc y') nsxMap with
               | None -> 
                  if not settings.IsAbstract then 
                     let inline relevantDom x' = loc x' = loc y' && cg.Graph.ContainsVertex x'
                     match doms.TryIsDominatedBy(x, relevantDom) with
                     | Some x' -> add x' y' (x, y)
                     | None -> 
                        (* if isOutside y' then 
                           match Seq.tryFind (coveringExternal y') nsx with
                           | None -> counterEx := Some(x, y, y')
                           | Some x' -> add x' y' (x, y)
                        else*)
                        counterEx := Some(x, y, y')
                  else counterEx := Some(x, y, y')
               | Some x' -> add x' y' (x, y)
      match !counterEx with
      | None -> Yes seen
      | Some cex -> No cex
   
   let getDuplicateNodes (cg : T) = 
      let ret = Dictionary()
      for v in cg.Graph.Vertices do
         let l = loc v
         let mutable value = Set.empty
         if ret.TryGetValue(l, &value) then ret.[l] <- Set.add v value
         else ret.[l] <- Set.singleton v
      Util.Dictionary.filter (fun _ v -> Set.count v > 1) ret
   
   let allDisjoint (cg : T) (dups : Dictionary<_, _>) = 
      let components = Dictionary() :> IDictionary<CgState, int>
      cg.Graph.WeaklyConnectedComponents(components) |> ignore
      dups |> Seq.forall (fun kv -> 
                 let szInit = Set.count kv.Value
                 let szFinal = Set.map (fun x -> components.[x]) kv.Value |> Set.count
                 szInit = szFinal)
   
   let getHardPreferences (cg : T) = 
      let cg = copyGraph cg
      cg.Graph.RemoveVertexIf(fun v -> isOutside v || not (isRealNode v)) |> ignore
      let dups = getDuplicateNodes cg
      if dups.Count = 0 || allDisjoint cg dups then Dictionary()
      else 
         let dups = Util.Dictionary.fold (fun acc _ v -> Set.union acc v) Set.empty dups
         let mustPrefer = Dictionary()
         for d in dups do
            let reach = Reachable.dfs cg d Down
            let below = Util.HashSet.filter (shadows d) reach
            if below.Count > 0 then mustPrefer.[d] <- below
         mustPrefer
   
   type PrefResult = 
      | Safe
      | Unsafe of CgState * CgState * CgState
   
   let isPreferred idx cg (cache : HashSet<_>) (doms : Domination.DominationTree) (x, y) = 
      let ce = CacheEntry(x.Id, y.Id)
      if cache.Contains ce then Safe
      else 
         match protect idx doms cg x y with
         | No(x, y, y') -> Unsafe(x, y, y')
         | Yes related -> 
            for n in related do
               let ce = CacheEntry(n.More.Id, n.Less.Id)
               ignore (cache.Add ce)
            Safe
   
   let mustBePreferred x y (mustPrefer : Dictionary<_, HashSet<_>>) = 
      match Util.Dictionary.tryFind x mustPrefer with
      | None -> false
      | Some ns -> ns.Contains(y)
   
   let getShortestPath (cg : T) (s : CgState) (d : CgState) = 
      let closure = cg.Graph.ShortestPathsDijkstra((fun _ -> 1.0), s)
      let hasPath, path = closure.Invoke d
      if hasPath then 
         path
         |> Seq.map (fun (e : Edge<CgState>) -> e.Target)
         |> List.ofSeq
         |> Some
      else None
   
   let stitchPath cg start vs = 
      List.fold (fun (patho, prev) v -> 
         match patho with
         | None -> (None, v)
         | Some path -> 
            match getShortestPath cg prev v with
            | None -> (None, v)
            | Some sp -> (Some(path @ sp), v)) (Some [], start) vs
      |> fst
   
   let pathHasLoop (vs : CgState list) = 
      let locs = List.fold (fun acc v -> Set.add v.Node.Loc acc) Set.empty vs
      Set.count locs <> List.length vs
   
   let createPathCounterExample (cg : T) (vs : CgState list) = 
      match vs with
      | [] -> failwith "unreachable"
      | hd :: tl -> 
         let patho = stitchPath cg hd tl
         match patho with
         | None -> None
         | Some path -> 
            if pathHasLoop path then None
            else patho
   
   let compare idx cg cache doms (mustPrefer : Dictionary<_, HashSet<_>>) comparedAsBetter x y = 
      let a = isPreferred idx cg cache doms (x, y)
      let b = isPreferred idx cg cache doms (y, x)
      match a, b with
      | Safe, Safe -> 0
      | Safe, Unsafe _ -> 
         comparedAsBetter := Set.add (x, y) !comparedAsBetter
         1
      | Unsafe _, Safe -> 
         comparedAsBetter := Set.add (y, x) !comparedAsBetter
         -1
      | Unsafe(l, m, n), Unsafe(o, p, q) -> 
         // see if they have different peers
         let nsx = 
            neighborsIn cg x
            |> Set.ofSeq
            |> Set.map loc
         
         let nsy = 
            neighborsIn cg y
            |> Set.ofSeq
            |> Set.map loc
         
         let xnoty = Set.difference nsx nsy
         let ynotx = Set.difference nsy nsx
         
         let ns = 
            if Set.count xnoty > 0 && Set.count ynotx > 0 then 
               let a = Set.minElement xnoty
               let b = Set.minElement ynotx
               Some(a, b)
            else None
         
         // if m <> n then use this pair, else use the p,q pair
         let example1 = 
            if m = n then createPathCounterExample cg [ cg.Start; m ]
            else createPathCounterExample cg [ cg.Start; m; n ]
         
         let example2 = 
            if p = q then createPathCounterExample cg [ cg.Start; p ]
            else createPathCounterExample cg [ cg.Start; p; q ]
         
         let example1 = Option.map (fun x -> (m, n, x)) example1
         let example2 = Option.map (fun x -> (p, q, x)) example2
         
         let example = 
            match example1, example2 with
            | None, _ | _, None -> None
            | Some(m, n, x), Some(p, q, y) -> Some((m, n, x), (p, q, y))
         raise (ConsistencyException(x, y, (ns, example)))
   
   let rec partition idx cg cache doms mustPrefer comparedAsBetter ((l, r) as acc) xs x = 
      match xs with
      | [] -> acc
      | hd :: tl -> 
         let cmp = compare idx cg cache doms mustPrefer comparedAsBetter x hd
         if cmp < 0 then partition idx cg cache doms mustPrefer comparedAsBetter (hd :: l, r) tl x
         else partition idx cg cache doms mustPrefer comparedAsBetter (l, hd :: r) tl x
   
   let rec qsort idx cg cache doms mustPrefer comparedAsBetter ls = 
      match ls with
      | [] -> []
      | x :: xs -> 
         let smaller, larger = partition idx cg cache doms mustPrefer comparedAsBetter ([], []) xs x
         let left = qsort idx cg cache doms mustPrefer comparedAsBetter smaller
         let right = qsort idx cg cache doms mustPrefer comparedAsBetter larger
         left @ [ x ] @ right
   
   let checkStrictPrefsEnforced idx cg cache doms (mustPrefer : Dictionary<_, HashSet<_>>) 
       comparedAsBetter = 
      for kv in mustPrefer do
         let x = kv.Key
         let ns = kv.Value
         for y in ns do
            if not <| Set.contains (x, y) !comparedAsBetter then 
               let a = isPreferred idx cg cache doms (x, y)
               let b = isPreferred idx cg cache doms (y, x)
               match a, b with
               | Unsafe _, _ | _, Safe -> // TODO: this is strictly required just make sure the preference is set right
                  raise (SimplePathException(x, y))
               | _, _ -> ()
   
   let findPrefAssignment idx cache doms cg mustPrefer nodes = 
      let comparedAsBetter = ref Set.empty
      
      let ret = 
         nodes
         |> List.ofSeq
         |> qsort idx cg cache doms mustPrefer comparedAsBetter
         |> Seq.ofList
      // quick sort will not ensure all strict preferences are satisfied
      // we have to check these additional constraints, but can reused cached results
      checkStrictPrefsEnforced idx cg cache doms mustPrefer comparedAsBetter
      ret
   
   let addForLabel idx cache doms ain cg mustPrefer (map : Dictionary<_, _>) l = 
      if Set.contains l ain then 
         if not (map.ContainsKey(l)) then 
            let nodes = Seq.filter (fun v -> loc v = l) cg.Graph.Vertices
            map.[l] <- findPrefAssignment idx cache doms cg mustPrefer nodes
      else map.[l] <- Seq.empty
      map
   
   let findOrdering idx cg : Result<Ordering, CounterExample> = 
      try 
         let mustPrefer = getHardPreferences cg
         let (ain, _) = Topology.alphabet cg.Topo
         let ain = Set.map (fun (v : Topology.Node) -> v.Loc) ain
         let doms = Domination.dominators cg cg.Start Down
         let cache = HashSet()
         
         let labels = 
            cg.Graph.Vertices
            |> Seq.choose (fun v -> 
                  if Topology.isTopoNode v.Node then Some(loc v)
                  else None)
            |> Set.ofSeq
         try 
            Ok(Set.fold (addForLabel idx cache doms ain cg mustPrefer) (Dictionary()) labels)
         with ConsistencyException(x, y, (ns, example)) -> Err((x, y, (ns, example)))
      with SimplePathException(x, y) -> Err((x, y, (None, None)))
   
   let findOrderingConservative (idx : int) = findOrdering idx

/// Sometimes we need to generate regex BGP filters
/// when dealing with external peers. We do this using
/// the standard automata-to-regex algorithm by walking 
/// backwards through the product graph from a given router
/// and constructing the regex matching any path to that router.
module ToRegex = 
   let constructRegex (cg : T) (state : CgState) : Regex.T = 
      let reMap = ref Map.empty
      let inline get v = Util.Map.getOrDefault v Regex.empty !reMap
      let inline add k v = reMap := Map.add k v !reMap
      let canReach = Reachable.dfs cg state Up
      add (cg.End, state) Regex.epsilon
      for e in cg.Graph.Edges do
         if canReach.Contains e.Target then 
            let r = 
               match e.Target.Node.Typ with
               | Topology.Unknown S -> Regex.out S
               | _ -> Regex.loc (loc e.Target)
            add (e.Target, e.Source) r
      let queue = Queue()
      for v in canReach do
         if isRealNode v then queue.Enqueue v
      canReach.Add cg.End |> ignore
      while queue.Count > 0 do
         let q = queue.Dequeue()
         for q1 in canReach do
            for q2 in canReach do
               if q1 <> q && q2 <> q then 
                  let x = get (q1, q2)
                  let y1 = get (q1, q)
                  let y2 = get (q, q)
                  let y3 = get (q, q2)
                  
                  let re = 
                     Regex.union x (Regex.concatAll [ y1
                                                      Regex.star y2
                                                      y3 ])
                  reMap := Map.add (q1, q2) re !reMap
         canReach.Remove q |> ignore
      (!reMap).[(cg.End, cg.Start)]

/// Module for reasoning about failures related to aggregation.
/// The idea is to:
/// (1)  pick a random path from src to aggregation point
/// (2)  remove the path from the product graph, and any 
///      edges related by the topology
/// (3)  repeat until aggregation is no longer reachable.
module Failure = 
   type FailType = 
      | NodeFailure of Topology.Node
      | LinkFailure of Topology.Node * Topology.Node
      override x.ToString() = 
         match x with
         | NodeFailure n -> "Node(" + n.Loc + ")"
         | LinkFailure(s, t) -> "Link(" + s.Loc + "," + t.Loc + ")"
   
   let allFailures n (topo : Topology.T) : seq<FailType list> = 
      let fvs = 
         Topology.vertices topo
         |> Seq.filter Topology.isInside
         |> Seq.map NodeFailure
      
      let fes = 
         Topology.edges topo
         |> Seq.filter (fun (src, tgt) -> Topology.isInside src || Topology.isInside tgt)
         |> Seq.map LinkFailure
      
      Seq.append fes fvs
      |> Seq.toList
      |> Util.List.combinations n
   
   let failedGraph (cg : T) (failures : FailType list) : T = 
      let failed = copyGraph cg
      
      let rec aux acc fs = 
         let (vs, es) = acc
         match fs with
         | [] -> acc
         | (NodeFailure s) :: tl -> aux (s.Loc :: vs, es) tl
         | (LinkFailure(s, t)) :: tl -> aux (vs, (s.Loc, t.Loc) :: (t.Loc, s.Loc) :: es) tl
      
      let (failedNodes, failedEdges) = aux ([], []) failures
      failed.Graph.RemoveVertexIf(fun v -> List.exists ((=) v.Node.Loc) failedNodes) |> ignore
      failed.Graph.RemoveEdgeIf(fun e -> List.exists ((=) (loc e.Source, loc e.Target)) failedEdges) 
      |> ignore
      failed
   
   let disconnect (cg : T) src dst (edgeMap : Dictionary<_, _>) : int = 
      let cg = copyGraph cg
      let mutable removed = 0
      let mutable hasPath = true
      while hasPath do
         match Reachable.path cg src dst with
         | None -> hasPath <- false
         | Some p -> 
            (* let sp = cg.Graph.ShortestPathsDijkstra((fun _ -> 1.0), src)
         let mutable path = Seq.empty
         ignore (sp.Invoke(dst, &path))
         match path with
         | null -> hasPath <- false
         | p -> *)
            removed <- removed + 1
            for e in p do
               let x = e.Source.Node.Loc
               let y = e.Target.Node.Loc
               let toRemove = edgeMap.[(x, y)]
               // printfn "remove"
               for e' in toRemove do
                  // printfn "  removing: (%s,%s)" (e.Source.Node.Loc) e.Target.Node.Loc
                  cg.Graph.RemoveEdge e' |> ignore
      removed
   
   let getEdgeLocMap (cg : T) = 
      let mutable edgeMap = Dictionary()
      for e in cg.Graph.Edges do
         let x = e.Source.Node.Loc
         let y = e.Target.Node.Loc
         let v = (x, y)
         let b, es = edgeMap.TryGetValue v
         if b then edgeMap.[v] <- e :: es
         else edgeMap.[v] <- [ e ]
      edgeMap
   
   let disconnectAll (cg : T) srcs dsts = 
      if Seq.isEmpty srcs || Seq.isEmpty dsts then None
      else 
         let edgeMap = getEdgeLocMap cg
         let mutable smallest = System.Int32.MaxValue
         let mutable pair = None
         for src in srcs do
            for dst in dsts do
               let k = disconnect cg src dst edgeMap
               if k < smallest then 
                  smallest <- k
                  pair <- Some(src, dst)
         let (x, y) = Option.get pair
         let k = max 0 (smallest - 1)
         Some(k, x.Node.Loc, y.Node.Loc)
   
   let disconnectLocs (cg : T) srcs dstLoc = 
      let dsts = Seq.filter (fun v -> loc v = dstLoc) cg.Graph.Vertices
      disconnectAll cg srcs dsts