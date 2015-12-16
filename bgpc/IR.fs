module IR

open System
open CGraph
open Common.Debug
open Common.Error

type CounterExample = 
    | UnusedPreferences of Map<int, Regex.T>
    | NoPathForRouters of Set<string>
    | InconsistentPrefs of CgState * CgState

type Match = 
    | Peer of string 
    | State of int array * string
    | PathRE of Regex.T
    | NoMatch

    override this.ToString () = 
        match this with 
        | Peer s -> "Peer=" + s
        | State(is,s) -> "Community=" + (List.ofArray is).ToString() + ", Peer=" + s
        | PathRE r -> "Regex=" + r.ToString()
        | NoMatch -> "--"

type Action = 
    | NoAction
    | SetComm of int array
    | SetMed of int
    | PrependPath of int

    override this.ToString() = 
        match this with 
        | NoAction -> ""
        | SetComm(is) -> "Community<-" + (List.ofArray is).ToString()
        | SetMed i -> "MED<-" + i.ToString()
        | PrependPath i -> "Prepend path " + i.ToString() + " times"

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

type DeviceConfig =
    {(* Prefix: Prefix.T; *)
     Originates: bool;
     Filters: (Import * Export list) list}

type T = Map<string, DeviceConfig>

let format (config: T) = 
    let sb = System.Text.StringBuilder ()
    for kv in config do 
        sb.Append("Router ") |> ignore
        sb.Append(kv.Key) |> ignore
        let deviceConf = kv.Value
        for ((m, lp), es) in deviceConf.Filters do
            sb.Append("\n  Import: ") |> ignore
            sb.Append(m.ToString()) |> ignore
            if (not deviceConf.Originates) && lp <> 100 then
                sb.Append(" (lp=" + lp.ToString() + ")") |> ignore
            for (peer, acts) in es do
                sb.Append("\n    Export to: " + peer) |> ignore
                sb.Append(", " + acts.ToString()) |> ignore
        sb.Append("\n\n") |> ignore
    sb.ToString()

(* Order config by router name, and then preference *)
let comparePrefThenLoc (x,i1) (y,i2) = 
    let cmp = compare i1 i2
    if cmp = 0 then
        compare x.Node.Loc y.Node.Loc
    else cmp

let genConfig (cg: CGraph.T) (ord: Consistency.Ordering) : T =
    let mutable config = Map.empty
    for entry in ord do 
        let mutable rules = []
        let loc = entry.Key
        let prefs = entry.Value 
        
        let mutable originates = false
        let mutable filters = []

        let prefNeighborsIn =
            prefs
            |> Seq.mapi (fun i v -> (neighborsIn cg v, i))
            |> Seq.map (fun (ns,i) -> Seq.map (fun n -> (n,i)) ns) 
            |> Seq.fold Seq.append Seq.empty 
            |> List.ofSeq
            |> List.sortWith comparePrefThenLoc

        (* Generate filters *)
        let mutable lp = 99
        let mutable lastPref = None
        for v, pref in prefNeighborsIn do 
            match lastPref with 
            | Some p when pref = p -> () 
            | _ ->
                lastPref <- Some pref 
                lp <- lp + 1
            let m =
                if Topology.isTopoNode v.Node 
                then Match.State(v.States, v.Node.Loc)
                else NoMatch
            let node = 
                neighbors cg v
                |> Seq.filter (fun x -> x.Node.Loc = loc) 
                |> Seq.head
            let exports =
                node
                |> neighbors cg
                |> Seq.filter (fun x -> Topology.isTopoNode x.Node)
                |> Seq.filter (fun x -> x.Node.Loc <> v.Node.Loc)
                |> Seq.toList
                |> List.map (fun x -> (x.Node.Loc, [SetComm(node.States)] ))

            filters <- ((m,lp), exports) :: filters
            originates <- v.Node.Typ = Topology.Start

        let deviceConf = {Originates=originates; Filters=filters}
        config <- Map.add loc deviceConf config
    config

let isCommunityTag (action: Action) : bool = 
    match action with
    | SetComm _ -> true
    | _ -> false

let rec getTag (actions: Action list) : (int array) option =
    match actions with 
    | [] -> None
    | hd::tl -> 
        match hd with 
        | SetComm is -> Some is 
        | _ -> getTag tl

let compressUniquePairs (cg: CGraph.T) (config: T) : T =
    (* Count of edges from a to b in product graph *)
    let pairCount = 
        cg.Graph.Vertices
        |> Seq.map ((fun v -> (v, neighbors cg v)) >> (fun (v, ns) -> Seq.map (fun n -> (v,n)) ns))
        |> Seq.fold Seq.append Seq.empty 
        |> Seq.groupBy (fun (a,b) -> (a.Node.Loc, b.Node.Loc))
        |> Seq.map (fun (k,sq) -> (k, Seq.length sq))
        |> Map.ofSeq
    let uniquePair (a,b) = 
        Map.find (a,b) pairCount = 1 
    (* Remove community matching/tagging for unique pairs *)
    let mutable newConfig = Map.empty
    for kv in config do
        let loc = kv.Key
        let deviceConf = kv.Value 
        let mutable newFilters = []
        for ((m,lp), es) in deviceConf.Filters do
            let m' = 
                match m with 
                | Match.State(is,x) -> if uniquePair (x,loc) then Match.Peer(x) else m
                | NoMatch -> NoMatch
                | _ -> failwith "not possible"
            let mutable newEs = []
            for (peer,acts) in es do
                let acts' = 
                    match getTag acts, uniquePair (loc,peer) with 
                    | None, _ | _, false -> acts
                    | Some is, true -> List.filter (isCommunityTag >> not) acts
                newEs <- (peer, acts') :: newEs
            let newFilt = ((m', lp), List.rev newEs)
            newFilters <- newFilt :: newFilters
        let newDeviceConf = {Originates=deviceConf.Originates; Filters=List.rev newFilters}
        newConfig <- Map.add loc newDeviceConf newConfig
    newConfig


let longestSequenceCommMatchesByPeer (dc: DeviceConfig) : Map<string, (int array) list> =
    let rec aux filters sequence bestCounts count (last,lastEs) mapping =
        match filters with
        | [] ->
            if last <> None then
                let peer = Option.get last
                let existing = Map.tryFind peer bestCounts
                if Option.isNone existing || Option.get existing < count 
                then Map.add peer sequence mapping 
                else mapping
            else mapping
        | ((m,lp),es)::fs ->
            let peer, is = 
                match m with 
                | Match.Peer(x) -> Some x, None
                | Match.State(is,x) -> Some x, Some is
                | _ -> None, None
            if Option.isNone peer then aux fs [] bestCounts 0 (None, None) mapping else
            let peer = Option.get peer
            if (Option.isNone last || (Option.get last = peer && Option.get lastEs = es)) then
                let sequence' =
                    match is with 
                    | None -> sequence
                    | Some is' -> is'::sequence
                aux fs sequence' bestCounts (count + 1) (Some peer, Some es) mapping
            else
                let lastPeer = Option.get last
                let existing = Map.tryFind lastPeer bestCounts
                if Option.isNone existing || Option.get existing < count then
                    aux fs [] (Map.add lastPeer count bestCounts) 0 (Some peer, Some es) (Map.add lastPeer sequence mapping)
                else aux fs [] bestCounts 0 (None, None) mapping

    aux dc.Filters [] Map.empty 0 (None, None) Map.empty


let findVertex cg (loc, states) = 
    cg.Graph.Vertices 
    |> Seq.find (fun v -> v.States = states && v.Node.Loc = loc)

let areAllIsomorphic cg (nodes: CGraph.CgState list) : bool =
    let rec aux fst nodes = 
        match nodes with
        | [] -> true
        | [x] -> Reachable.supersetPaths (cg, x) (cg, fst)
        | x::((y::z) as tl) -> (Reachable.supersetPaths (cg, x) (cg, y)) && (aux fst (y::z))
    match nodes with
    | [] -> true
    | [x] -> false
    | x::y::z -> aux x nodes

let removeCommFiltersForPeer (peer: string) (dc: DeviceConfig) : DeviceConfig =
    let replace arg =
        let ((m,lp), es) = arg
        match m with
        | Match.State(is,x) when x = peer -> ((Match.Peer(x),lp), es)
        | _ -> arg
    let newFilters = List.map replace dc.Filters
    {Originates=dc.Originates; Filters=newFilters}

let compressIsomorphicImports (cg: CGraph.T) (config: T) : T =
    let mutable newConfig = Map.empty
    for kv in config do
        let loc = kv.Key
        let deviceConf = kv.Value 
        let mByPeer = longestSequenceCommMatchesByPeer deviceConf
        let mutable newDC = deviceConf
        for kv in mByPeer do
            let peer = kv.Key
            let nodes = 
                kv.Value
                |> List.map (fun c -> findVertex cg (peer,c))
                |> List.map (fun v -> Seq.filter (fun v' -> v'.Node.Loc = loc) (neighbors cg v) |> Seq.head)
            if areAllIsomorphic cg nodes then
                newDC <- removeCommFiltersForPeer peer newDC
        newConfig <- Map.add loc newDC newConfig
    newConfig

let compressIdenticalImports (config: T) : T =
    for kv in config do
        let loc = kv.Key 
        let deviceConf = kv.Value
        let foo =deviceConf.Filters
        
        ()

    failwith ""



let compressTaggingWhenNotImported (config: T) : T =
    let importCommNeeded = ref Set.empty 
    for kv in config do 
        let loc = kv.Key 
        let deviceConf = kv.Value 
        for ((m,_), _) in deviceConf.Filters do
            match m with 
            | Match.State(_,x) -> 
                importCommNeeded := Set.add (loc,x) !importCommNeeded
            | _ -> ()
    Map.map (fun loc dc ->
        let filters' = 
            List.map (fun ((m,lp), es) -> 
                let es' = 
                    List.map (fun (peer,acts) -> 
                        if Set.contains (peer, loc) !importCommNeeded then (peer,acts)
                        else (peer, List.filter (isCommunityTag >> not) acts)
                    ) es
                ((m,lp), es')
            ) dc.Filters
        {Originates=dc.Originates; Filters=filters'}
    ) config



let compress (cg: CGraph.T) (config: T) (outName: string) : T =
    let configMin =
        config
        |> compressUniquePairs cg
        |> compressIsomorphicImports cg
        |> compressTaggingWhenNotImported
    debug1 (fun () -> System.IO.File.WriteAllText(outName + ".ir", format config))
    debug1 (fun () -> System.IO.File.WriteAllText(outName + "-min.ir", format configMin))
    configMin

let compileToIR (topo: Topology.T) (reb: Regex.REBuilder) (res: Regex.T list) (outName: string) : Result<T, CounterExample> =
    let cg = CGraph.buildFromRegex topo reb res
    debug1 (fun () -> CGraph.generatePNG cg outName)
    (* Ensure the path suffix property and dont conside simple paths *)
    CGraph.Minimize.delMissingSuffixPaths cg
    CGraph.Minimize.minimizeO3 cg
    (* Save graphs to file *)
    debug1 (fun () -> CGraph.generatePNG cg (outName + "-min"))
    (* Check for errors *)
    let startingLocs = List.fold (fun acc r -> Set.union (reb.StartingLocs r) acc) Set.empty res
    let originators = 
        CGraph.neighbors cg cg.Start
        |> Seq.map (fun v -> v.Node.Loc)
        |> Set.ofSeq
    let canOriginate = 
        cg.Topo.Vertices 
        |> Seq.filter Topology.canOriginateTraffic
        |> Seq.map (fun v -> v.Loc) 
        |> Set.ofSeq
    let locsThatNeedPath = Set.difference (Set.intersect startingLocs canOriginate) originators
    let locsThatGetPath = CGraph.acceptingLocations cg
    logInfo1(String.Format("Locations that need path: {0}", locsThatNeedPath.ToString()))
    logInfo1(String.Format("Locations that get path: {0}", locsThatGetPath.ToString()))
    let lost = Set.difference locsThatNeedPath locsThatGetPath
    if not (Set.isEmpty lost) then 
        Err(NoPathForRouters(lost))
    else
        (* Find unused preferences *)
        let numberedRegexes = seq {for i in 1.. List.length res do yield i}  |> Set.ofSeq
        let prefs = CGraph.preferences cg
        let unusedPrefs = Set.difference numberedRegexes prefs
        if not (Set.isEmpty unusedPrefs) then
            let cexamples = Set.fold (fun acc p -> Map.add p (List.nth res (p-1)) acc) Map.empty unusedPrefs
            Err(UnusedPreferences(cexamples))
        else
            match Consistency.findOrderingConservative cg outName with 
            | Ok ord ->
                let config = genConfig cg ord
                let config = compress cg config outName
                Ok (config)
            | Err((x,y)) -> Err(InconsistentPrefs(x,y))
