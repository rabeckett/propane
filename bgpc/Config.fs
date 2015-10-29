module Config

open Extension.Error
open CGraph

type Match = 
    | Peer of string 
    | State of int array * string
    | PathRE of Regex.T

type Action = 
    | NoAction
    | SetComm of int array * string
    | SetMed of int
    | SetLP of int

type Actions = Action list

type Rule =
    {Import: Match;
     Export: Actions}

type T = Map<string, Rule list>


exception PrefConsistencyException of CgState * CgState 
exception TopoConsistencyException of CgState * CgState

type ConsistencyViolation = 
    | PrefConsistency of CgState * CgState
    | TopoConsistency of CgState * CgState

type Preferences = (CgState * Set<int>) list

type Ordering = 
    Map<string, Preferences>


let print (config: T) = 
    for kv in config do 
        printfn "\nRouter %s" kv.Key
        for rule in kv.Value do 
            printfn "  Match: (%A), Update: (%A)" rule.Import rule.Export



let private prefConsistency (cg: CGraph.T) : Result<Ordering, ConsistencyViolation> =

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
        let accepting = Reachable.srcAccepting cg v
        acc <- Map.add loc ((v, accepting)::ins) acc 
    
    (* Ensure well-formedness and return the ordering. Raises an exception otherwise *) 
    let check _ v = aux (List.sortWith comparer v)

    (* Hide the exception behind a result type *)
    try Ok (Map.map check acc)
    with PrefConsistencyException (x,y) ->
        Err (PrefConsistency (x,y))


let private topoConsistency (cg: CGraph.T) (ord: Ordering) : Result<unit, ConsistencyViolation> =

    let checkFailures loc x y = 
        (* failing links from x disconnects y from start to accept state *)
        let es = Reachable.edges cg x
        if not (Set.isEmpty es) && (Option.isSome x.Accept) then 
            let copy = copyGraph cg 
            for (u,v) in es do 
                copy.Graph.RemoveEdgeIf (fun e -> 
                    (e.Source.Topo.Loc = u.Topo.Loc && e.Target.Topo.Loc = v.Topo.Loc) || 
                    (e.Target.Topo.Loc = u.Topo.Loc && e.Source.Topo.Loc = v.Topo.Loc)
                ) |> ignore

            (* Check reachability for y *)
            if Reachable.srcDst copy copy.Start y then
                if not (Set.isEmpty (Reachable.srcAccepting copy y)) then 
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



let private genConfig (cg: CGraph.T) (ord: Ordering) : T = 
    
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
                if unambiguous then Peer v.Topo.Loc 
                else State (v.States, v.Topo.Loc)

            let a = 
                if lp = 100 then [] 
                else [SetLP(lp)]

            rules <- {Import = m; Export = a}::rules
        
        config <- Map.add loc rules config

    config


(* Generate the BGP match/action rules that are guaranteed to 
   implement the user policy under all possible failure scenarios. 
   This function returns an intermediate representation (IR) for BGP policies *) 
let compile (cg: CGraph.T) : Result<T, ConsistencyViolation> =
    match prefConsistency cg with 
    | Ok ord ->
        match topoConsistency cg ord with 
        | Ok _ -> Ok (genConfig cg ord)
        | Err(tc) -> Err(tc)
    | Err(pc) -> Err(pc)





(* Generate templates 
let generateTemplates (config: T) (path: string) = 
    failwith "todo" *)
