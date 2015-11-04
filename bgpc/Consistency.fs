module Consistency
open Extension.Error
open CGraph
open QuickGraph

exception PrefConsistencyException of CgState * CgState 
exception TopoConsistencyException of CgState * CgState

type CounterExample = 
    | PrefViolation of CgState * CgState
    | TopoViolation of CgState * CgState

type Preferences = (CgState * Set<int>) list

type Ordering = 
    Map<string, Preferences>

let findOrdering (cg: CGraph.T) : Result<Ordering, CounterExample> =
    let compareByMinThenRange (_,x) (_,y) = 
        let minx, maxx = Set.minElement x, Set.maxElement x
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
                printfn "x: %A" x
                printfn "y: %A" y
                raise (PrefConsistencyException (vx, vy))
            else  hd :: (wfPrefRanges tl)
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

let checkFailures (cg: CGraph.T) (ord: Ordering) : Result<unit, CounterExample> =
    let cgRev = copyReverseGraph cg
    let subsumes x y = 
        (Reachable.supersetPaths (cg, x) (cg, y)) ||
        (Reachable.supersetPaths (cg, x) (cgRev, y)) || 
        (Reachable.supersetPaths (cgRev, x) (cg, y)) || 
        (Reachable.supersetPaths (cgRev, x) (cgRev, y))
    (* TODO: Is this check needed if they have equal prefs? *)
    let rec checkAll name prefs = 
        match prefs with 
        | [] | [_] -> ()
        | (x,is)::(((y,js)::_) as tl) -> 
            if not (subsumes x y) then 
                raise (TopoConsistencyException (x,y))
            checkAll name tl
    try Ok (Map.iter checkAll ord)
    with TopoConsistencyException(x,y) -> 
        Err (TopoViolation(x,y))

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

let checkFailuresByEnumerating (n:int) (topo: Topology.T) (cg: CGraph.T) (ord: Ordering) : Result<unit, CounterExample> = 
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

