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


let print (config: T) = 
    for kv in config do 
        printfn "\nRouter %s" kv.Key
        for rule in kv.Value do 
            printfn "  Match: (%A), Update: (%A)" rule.Import rule.Export

let private genConfig (cg: CGraph.T) (ord: Consistency.Ordering) : T =     
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

let compile (topo: Topology.T) (cg: CGraph.T) : Result<T, Consistency.CounterExample> =
    match Consistency.findOrdering cg with 
    | Ok ord ->
        match Consistency.checkFailures cg ord with 
        | Ok _ -> Ok (genConfig cg ord)
        | Err(tc) -> Err(tc)
            (* let nFailures = 1
            match Consistency.checkFailuresByEnumerating nFailures topo cg ord with 
            | Ok _ -> Ok (genConfig cg ord)
            | Err(tc) -> Err(tc) *)
    | Err(pc) -> Err(pc)


(* Generate templates 
let generateTemplates (config: T) (path: string) = 
    failwith "todo" *)
