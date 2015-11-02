module Consistency

open Extension.Error
open CGraph
open QuickGraph


(* Local control flow exceptions *)
exception PrefConsistencyException of CgState * CgState 
exception TopoConsistencyException of CgState * CgState

(* An explanation for being unimplementable in BGP *)
type CounterExample = 
    | PrefViolation of CgState * CgState
    | TopoViolation of CgState * CgState

(* Preference ranking for each router based on possible routes *)
type Preferences = (CgState * Set<int>) list

(* Preferences for each internal router *)
type Ordering = 
    Map<string, Preferences>


(* Check if the routers can make local decisions not knowing about failures *)
let findOrdering (cg: CGraph.T) : Result<Ordering, CounterExample> =

    let compareByMinThenRange (_,x) (_,y) = 
        let minx, maxx = Set.minElement x, Set.maxElement y
        let miny, maxy = Set.minElement y, Set.maxElement y
        let cmp = compare minx miny
        if cmp = 0 then 
            compare (maxx - minx) (maxy - miny)
        else cmp
    
    let rec wfPrefRanges ls = 
        match ls with
        | [] | [_] -> ls
        | ((vx,x) as hd)::(( (vy,y)::_) as tl) ->
            let maxx = Set.maxElement x 
            let miny = Set.minElement y
            if maxx > miny then
                raise (PrefConsistencyException (vx, vy))
            else 
                hd :: (wfPrefRanges tl)
    
    let mutable acc = Map.empty
    for v in cg.Graph.Vertices do 
        let loc = v.Topo.Loc
        let ins = if Map.containsKey loc acc then Map.find loc acc else []
        let accepting = Reachable.srcAccepting cg v
        acc <- Map.add loc ((v, accepting)::ins) acc 
    
    let check _ v = wfPrefRanges (List.sortWith compareByMinThenRange v)

    try Ok (Map.map check acc)
    with PrefConsistencyException (x,y) ->
        Err (PrefViolation (x,y))


(* Build a copy of the constraint graph with nodes and edges removed to simulate failures *)
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
    failed.Graph.RemoveVertexIf (fun v -> List.exists ((=) v.Topo.Loc) failedNodes) |> ignore
    failed.Graph.RemoveEdgeIf (fun e -> List.exists ((=) (e.Source.Topo.Loc, e.Target.Topo.Loc)) failedEdges) |> ignore
    failed


(* Conservative check that no failure scenario will result in unspecified routes *)
let checkFailures () = 
    failwith "todo"


(* Precise check that no failure scenario will result in unspecified routes *)
let checkFailuresByEnumerating (n:int) (topo: Topology.T) (cg: CGraph.T) (ord: Ordering) = 
    let aux () = 
        let failCombos = Topology.Failure.allFailures n topo
        for fails in failCombos do 
            let cgFailed1 = failedGraph cg fails
            let cgFailed2 = copyGraph cgFailed1
            Minimize.removeNodesNotOnAnySimplePathToEnd cgFailed1
            Minimize.removeNodesNotReachableOnSimplePath cgFailed2
            for v in cgFailed1.Graph.Vertices do 
                match Seq.tryFind (fun v' -> v'=v) cgFailed2.Graph.Vertices with 
                | Some _ -> ()
                | None -> 
                    match Seq.tryFind (fun v' -> v'.Topo.Loc = v.Topo.Loc && v'<>v) cgFailed2.Graph.Vertices with
                    | None -> ()
                    | Some v' -> raise (TopoConsistencyException (v,v'))
    try Ok (aux ()) 
    with TopoConsistencyException(v,u) -> 
        Err (TopoViolation(v,u))

