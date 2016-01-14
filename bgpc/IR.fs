module IR

open CGraph
open Common.Debug
open Common.Error

exception UncontrollableEnterException of string
exception UncontrollablePeerPreferenceException of string

type CounterExample = 
    | UnusedPreferences of Map<int, Regex.T>
    | NoPathForRouters of Set<string>
    | InconsistentPrefs of CgState * CgState
    | UncontrollableEnter of string
    | UncontrollablePeerPreference of string

type Match = 
    | Peer of string 
    | State of string * string
    | PathRE of Regex.T
    | NoMatch

    override this.ToString () = 
        match this with 
        | Peer s -> "Peer=" + s
        | State(is,s) -> "Community=" + is + ", Peer=" + s
        | PathRE r -> "Regex=" + r.ToString()
        | NoMatch -> "--"

type Action = 
    | NoAction
    | SetComm of string
    | SetMed of int
    | PrependPath of int

    override this.ToString() = 
        match this with 
        | NoAction -> ""
        | SetComm(is) -> "Community<-" + is
        | SetMed i -> "MED<-" + i.ToString()
        | PrependPath i -> "Prepend " + i.ToString()

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

type DeviceConfig =
    {Originates: bool;
     Filters: (Import * Export list) list}

type PredConfig = Predicate.T * Map<string, DeviceConfig>

type DeviceAggregates = (Prefix.T list * seq<string>) list
type Aggregates = Map<string, DeviceAggregates>

type RouterConfig = 
    {Actions: (Predicate.T * DeviceConfig) list;
     Aggregates: DeviceAggregates}

type T = Map<string, RouterConfig>

let joinConfigs aggs (pairs: PredConfig list) : T =
    let mutable result = Map.empty
    for (prefix, config) in pairs do 
        for kv in config do
            let value = (prefix, kv.Value)
            match Map.tryFind kv.Key result with
            | None -> result <- Map.add kv.Key [value] result
            | Some x -> result <- Map.add kv.Key (x @ [value]) result
    Map.map (fun router vs ->
        match Map.tryFind router aggs with
        | None -> {Actions=vs; Aggregates=[]}
        | Some xs -> {Actions=vs; Aggregates=xs}) result

let format (config: T) = 
    let sb = System.Text.StringBuilder ()
    for kv in config do
        let routerName = kv.Key 
        let routerConfig = kv.Value
        sb.Append("\nRouter ") |> ignore
        sb.Append(routerName) |> ignore
        for (prefix, peers) in routerConfig.Aggregates do
            for peer in peers do
                sb.Append("\n  Aggregate(" + string prefix + ", " + peer + ")") |> ignore
        for (pred, deviceConf) in routerConfig.Actions do
            let predStr = string pred
            for ((m, lp), es) in deviceConf.Filters do
                sb.Append("\n  Match: ") |> ignore
                sb.Append("[Prefix=" + predStr + ", ") |> ignore
                sb.Append(m.ToString() + "]") |> ignore
                if (not deviceConf.Originates) && lp <> 100 then
                    sb.Append(" (LP=" + lp.ToString() + ")") |> ignore
                for (peer, acts) in es do
                    sb.Append("\n    Export: [Peer<-" + peer) |> ignore
                    if acts <> [] then
                        let str = 
                            acts
                            |> List.map (fun a -> a.ToString())
                            |> Common.List.joinBy ","
                        sb.Append(", " + str) |> ignore
                    sb.Append("]") |> ignore
        sb.Append("\n") |> ignore
    sb.ToString()

let formatPrefix pconfig = 
    format (joinConfigs Map.empty [pconfig])

(* Ensure well-formedness for controlling 
   traffic entering the network. MED and prepending allow
   certain patterns of control to immediate neighbors only *)

type IncomingPattern = 
    | Anything
    | Nothing of string
    | Specific of Regex.T

type IncomingInfo = 
    {Peers: seq<CgState>;
     Info: Map<CgState, IncomingPattern>}

type IncomingExportMap = Map<CgState, Action list>

let collectForPeer cg acc peer = 
    let reachable = 
        Reachable.src cg peer Down
        |> Set.filter (fun x -> x <> peer && Topology.isTopoNode x.Node)
    let hasRepeatedOut = Set.exists (CGraph.isRepeatedOut cg) reachable
    let hasOther = (reachable.Count > 1) || (not hasRepeatedOut && reachable.Count > 0)
    match hasRepeatedOut, hasOther with
    | false, false -> Map.add peer (Nothing peer.Node.Loc) acc
    | true, false -> Map.add peer Anything acc
    | _, true ->
        let cexample = CGraph.ToRegex.constructRegex cg peer
        Map.add peer (Specific cexample) acc 

let collectIncomingInfo (cg: CGraph.T) : IncomingInfo =
    let isExportPeer v = 
        Topology.isOutside v.Node && 
        Seq.exists (fun u -> Topology.isInside u.Node) (neighborsIn cg v)
    let exportPeers = Seq.filter isExportPeer cg.Graph.Vertices
    let info = Seq.fold (collectForPeer cg) Map.empty exportPeers
    {Peers = exportPeers; Info = info}

let getUnique peers =
    Set.ofSeq (Seq.map (fun p -> p.Node.Loc) peers)

let addExports (settings: Args.T) info peers actions exportMap =
    let mutable exportMap = exportMap
    let mutable actions = actions
    for p in peers do
        match Map.find p info.Info with 
        | Anything -> ()
        | Nothing x ->
            if settings.UseNoExport then 
                actions <- (SetComm "no-export") :: actions
            else raise (UncontrollableEnterException ("enable no-export to limit incoming traffic to peer: " + x))
        | Specific re -> 
            raise (UncontrollableEnterException ("incoming traffic cannot conform to: " + re.ToString()))
        exportMap <- Map.add p actions exportMap
    exportMap

let configureIncomingTraffic cg : IncomingExportMap =
    let settings = Args.getSettings()
    let info = collectIncomingInfo cg
    let byPreference =
        info.Peers 
        |> Seq.map (fun p -> (Set.minElement (Reachable.srcAccepting cg p Down), p))
        |> Seq.groupBy fst
        |> Seq.map (fun (x,y) -> (x, Seq.map snd y))
        |> Seq.sortBy fst
    let mutable exportMap = Map.empty
    let mutable i = 0
    let mutable prev = None
    for (_, peers) in byPreference do
        match prev with
        | None -> 
            exportMap <- addExports settings info peers [] exportMap
            prev <- Some peers
        | Some ps ->
            let unqNow = getUnique peers
            let unqPrev = getUnique ps
            let pre = Set.minElement unqPrev
            let now = Set.minElement unqNow
            let canAvoidAggregation = (Set.count unqPrev = 1) && (Set.count unqNow = 1) && (now = pre)
            if canAvoidAggregation && (settings.UseMed || settings.UsePrepending) then
                let mutable actions = []
                if settings.UsePrepending && i > 0 then actions <- (PrependPath (3*i)) :: actions
                if settings.UseMed && i > 0 then actions <- (SetMed (80+i)) :: actions
                exportMap <- addExports settings info peers actions exportMap
            else
                (* TODO: we need to use aggregates here since last time there were many *)
                raise (UncontrollablePeerPreferenceException now)
                ()
            prev <- Some peers
        i <- i + 1
    exportMap


(* Order config by preference and then router name. 
   This makes it easier to minimize the config *)
let comparePrefThenLoc (x,i1) (y,i2) = 
    let cmp = compare i1 i2
    if cmp = 0 then
        compare x.Node.Loc y.Node.Loc
    else cmp

(* Remap community values to be more sensible *)
let initMapper () =
    let i = ref 0
    let commMapper = ref Map.empty
    (fun comm -> 
        match Map.tryFind comm !commMapper with
        | None -> 
            i := !i + 1
            commMapper := Map.add comm !i !commMapper 
            !i
        | Some x -> x) 

(* Generate the configuration given preference-based ordering 
   that satisfies our completeness/fail resistance properties *)
let genConfig (cg: CGraph.T) (pred: Predicate.T) (ord: Consistency.Ordering) (inExports: IncomingExportMap) : PredConfig =
    let settings = Args.getSettings ()
    let (ain, _) = Topology.alphabet cg.Topo
    let ain = Set.map (fun (v: Topology.State) -> v.Loc) ain
    let commMapper = initMapper ()
    let mutable config = Map.empty
    for entry in ord do 
        let mutable rules = []
        let loc = entry.Key
        let prefs = entry.Value 
        (* Only generate config for internal locations *)
        if ain.Contains loc then
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
            let mutable lp = 101
            let mutable lastPref = None
            for v, pref in prefNeighborsIn do 
                match lastPref with 
                | Some p when pref = p -> () 
                | _ ->
                    lastPref <- Some pref 
                    lp <- lp - 1
                let m =
                    if Topology.isTopoNode v.Node then
                        if not (ain.Contains v.Node.Loc) then
                            let re = CGraph.ToRegex.constructRegex (CGraph.copyReverseGraph cg) v
                            match Regex.isLoc re with
                            | Some x -> Match.Peer x 
                            | _ -> Match.PathRE re
                        else Match.State(string (commMapper v.States), v.Node.Loc)
                    else NoMatch
                let node = 
                    neighbors cg v
                    |> Seq.filter (fun x -> x.Node.Loc = loc) 
                    |> Seq.head
                let expNeighbors = 
                    neighbors cg node
                    |> Seq.toList
                    |> List.filter (fun x -> x.Node.Loc <> v.Node.Loc && Topology.isTopoNode x.Node)
                
                (* Set exports *)
                let mutable exports = Map.empty
                for n in expNeighbors do
                    let loc = n.Node.Loc
                    if Topology.isOutside n.Node then
                        exports <- Map.add loc (Map.find n inExports) exports
                    else exports <- Map.add loc [SetComm(string (commMapper node.States))] exports

                filters <- ((m,lp), Map.toList exports) :: filters
                originates <- v.Node.Typ = Topology.Start

            let deviceConf = {Originates=originates; Filters=filters}
            config <- Map.add loc deviceConf config
    (pred, config)


/// Compress configs to make them more readable by 
/// e.g., removing unnecessary community tagging 
/// or making regexes more readable etc
module Compress =

    let isCommunityTag (action: Action) : bool = 
        match action with
        | SetComm _ -> true
        | _ -> false

    let rec getTag (actions: Action list) : string option =
        match actions with 
        | [] -> None
        | hd::tl -> 
            match hd with 
            | SetComm is -> Some is 
            | _ -> getTag tl

    (* For each pair of (X,Y) edges in the product graph
       if the pair is unique, then there is no need to 
       match/tag on the community, so we can remove it *)
    let compressUniquePairs (cg: CGraph.T) ((pred, config): PredConfig) : PredConfig =
        let pairCount = 
            cg.Graph.Vertices
            |> Seq.map ((fun v -> (v, neighbors cg v)) >> (fun (v, ns) -> Seq.map (fun n -> (v,n)) ns))
            |> Seq.fold Seq.append Seq.empty 
            |> Seq.countBy (fun (a,b) -> (a.Node.Loc, b.Node.Loc))
            |> Map.ofSeq

        let uniquePair (a,b) = 
            Map.find (a,b) pairCount = 1 

        let mutable newConfig = Map.empty
        for kv in config do
            let loc = kv.Key
            let deviceConf = kv.Value 
            let mutable newFilters = []
            for ((m,lp), es) in deviceConf.Filters do
                let m' = 
                    match m with 
                    | Match.State(is,x) -> if uniquePair (x,loc) then Match.Peer(x) else m
                    | _ -> m
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
        (pred, newConfig)

    (*
    (* Find the longest consecutive sequence of imports for each peer. 
       Only imports where the community is matched are counted towards the sequence.
       This makes a single pass over the list to find the largest sequence for each peer *)
    let longestSequenceCommMatchesByPeer (dc: DeviceConfig) : Map<string, string list> =
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
                    let sequence', inc =
                        match is with 
                        | None -> sequence, 0
                        | Some is' -> (is'::sequence), 1
                    aux fs sequence' bestCounts (count + inc) (Some peer, Some es) mapping
                else
                    let lastPeer = Option.get last
                    let existing = Map.tryFind lastPeer bestCounts
                    let sequence' =
                        match is with 
                        | None -> []
                        | Some is' -> [is']
                    if Option.isNone existing || Option.get existing < count then
                        aux fs sequence' (Map.add lastPeer count bestCounts) 0 (Some peer, Some es) (Map.add lastPeer sequence mapping)
                    else aux fs sequence' bestCounts 0 (Some peer, Some es) mapping

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
        | [x] -> true
        | x::y::z -> aux x nodes

    let removeCommFiltersForStates (states: (string * string) list) (dc: DeviceConfig) : DeviceConfig =
        let relevant x = 
            List.exists ((=) x ) states
        let replace arg =
            let ((m,lp), es) = arg
            match m with
            | Match.State(is,x) when relevant (x,is) -> ((Match.Peer(x),lp), es)
            | _ -> arg
        let newFilters = List.map replace dc.Filters
        {Originates=dc.Originates; Filters=newFilters}

    (* Removes redundant community matches when ultimately nobody else
       will ever care about the difference in community value.
       We have to be careful to avoid removing the community match when
       preferences are separated by an intermediate preference since the community 
       may be used to distinguish between these.
       Uses longestSequenceCommMatchesByPeer to ensure this property. *)
    let compressIsomorphicImports (cg: CGraph.T) ((prefix, config): T) : T =
        let mutable newConfig = Map.empty
        for kv in config do
            let loc = kv.Key
            let deviceConf = kv.Value 
            let mByPeer = longestSequenceCommMatchesByPeer deviceConf
            let mutable newDC = deviceConf
            for kv in mByPeer do
                let peer = kv.Key
                let states = 
                    kv.Value
                    |> List.map (fun c -> (peer,c))
                let nodes = 
                    states
                    |> List.map (findVertex cg)
                    |> List.map (fun v -> Seq.filter (fun v' -> v'.Node.Loc = loc) (neighbors cg v) |> Seq.head)
                if (List.length nodes) > 1 && (areAllIsomorphic cg nodes) then
                    newDC <- removeCommFiltersForStates states newDC
            newConfig <- Map.add loc newDC newConfig
        (prefix, newConfig) *)


    (* Removes less preferred, but otherwise identical imports.
       These can never occur unless they are back-to-back in the configuration. *)
    let compressIdenticalImports ((pred, config): PredConfig) : PredConfig =
        let rec aux filters acc = 
            match filters with
            | [] -> acc
            | (((m,lp), es) as f)::fs ->
                let cleaned = List.filter (fun ((m',_),_) -> m' <> m) fs
                aux cleaned (f::acc)
        let mutable newConfig = Map.empty
        for kv in config do
            let loc = kv.Key 
            let deviceConf = kv.Value
            let filters = deviceConf.Filters
            let mutable newFilters = []
            let newFilters = aux (List.rev deviceConf.Filters) []
            let newDeviceConf = {Originates=deviceConf.Originates; Filters=newFilters}
            newConfig <- Map.add loc newDeviceConf newConfig
        (pred, newConfig)

    (* Avoids tagging with a community from A to B, when B never
       needs to distinguish based on that particular community value *)
    let compressTaggingWhenNotImported ((pred, config): PredConfig) : PredConfig =
        let importCommNeeded = ref Map.empty 
        for kv in config do 
            let loc = kv.Key 
            let deviceConf = kv.Value 
            for ((m,_), _) in deviceConf.Filters do
                match m with 
                | Match.State(is,x) ->
                    let edge = (loc,x)
                    match Map.tryFind edge !importCommNeeded with
                    | None -> importCommNeeded := Map.add (loc,x) (Set.singleton is) !importCommNeeded
                    | Some iss -> importCommNeeded := Map.add (loc,x) (Set.add is iss) !importCommNeeded
                | _ -> ()
        let newConfig = 
            Map.map (fun loc dc ->
                let filters' = 
                    List.map (fun ((m,lp), es) ->
                        let es' = 
                            List.map (fun (peer,acts) ->
                                let acts' = 
                                    List.filter (fun a ->
                                        match a with
                                        | SetComm is ->
                                            Map.containsKey (peer,loc) !importCommNeeded &&
                                            !importCommNeeded
                                            |> Map.find (peer,loc)
                                            |> Set.contains is
                                        | _ -> true
                                    ) acts
                                (peer,acts')
                            ) es
                        ((m,lp), es')
                    ) dc.Filters
                {Originates=dc.Originates; Filters=filters'}
            ) config
        (pred, newConfig)

    (* Whenever we export identical routes to all peers, we can replace
       this with a simpler route export of the form: (Export: * ) for readability *)
    let compressAllExports (cg: CGraph.T) ((pred, config): PredConfig) : PredConfig =
        let mutable newConfig = Map.empty
        for kv in config do
            let loc = kv.Key 
            let deviceConf = kv.Value
            let filters = deviceConf.Filters
            let mutable newFilters = []
            let topoNode = 
                cg.Topo.Vertices
                |> Seq.find (fun v -> v.Loc = loc)
            for f in filters do 
                let ((m,lp), es) = f 
                let topoPeers = 
                    cg.Topo.OutEdges topoNode
                    |> Seq.map (fun e -> e.Target.Loc)
                let eqActions =
                    es
                    |> List.map snd 
                    |> Set.ofList
                    |> Set.count
                    |> ((=) 1)
                let import = 
                    match m with 
                    | Match.Peer(x) -> Some x
                    | _ -> None
                let coversPeer p = 
                    (Option.isSome import && p = Option.get import) || 
                    (List.exists (fun  (peer,_) -> peer = p) es)
                let allPeers = 
                    topoPeers
                    |> Seq.forall coversPeer
                if eqActions && allPeers && List.length es <= Seq.length topoPeers then 
                    newFilters <- ((m,lp), [("*", snd es.Head)]) :: newFilters
                else newFilters <- f :: newFilters
            let newDC = {Originates=deviceConf.Originates; Filters=List.rev newFilters}
            newConfig <- Map.add loc newDC newConfig
        (pred, newConfig)

    (* Whenever import identical routes from all peers, we can replace
       this with a simpler route import of the form: (Import: * ) for readability *)
    let compressAllImports (cg: CGraph.T) ((pred, config): PredConfig) : PredConfig =
        let mutable newConfig = Map.empty
        for kv in config do
            let loc = kv.Key 
            let deviceConf = kv.Value
            let filters = deviceConf.Filters
            let mutable newFilters = []
            let topoNode = 
                cg.Topo.Vertices
                |> Seq.find (fun v -> v.Loc = loc)
            let topoPeers = 
                cg.Topo.OutEdges topoNode
                |> Seq.map (fun e -> e.Target.Loc)
            let eqActions =
                filters
                |> List.map snd 
                |> Set.ofList
                |> Set.count
                |> ((=) 1)
            let eqLP = 
                filters
                |> List.map (fst >> snd)
                |> Set.ofList
                |> Set.count
                |> ((=) 1)
            let noComms = 
                filters
                |> List.map (fst >> fst)
                |> List.forall (fun x -> match x with Match.Peer _ -> true | _ -> false)
            let coversPeer p = 
                List.exists (fun  ((m,lp),es) -> 
                    match m with
                    | Match.Peer(x) -> x = p
                    | _ -> false
                ) filters
            let allPeers = 
                topoPeers
                |> Seq.forall coversPeer
            let allSame = eqActions && eqLP && noComms && allPeers
            let newFilters = 
                if allSame && List.length filters = Seq.length topoPeers && not (List.isEmpty filters)
                then
                    let ((m,lp),es) = List.head filters 
                    [((Match.Peer("*"), lp), es)]
                else filters
            let newDC = {Originates=deviceConf.Originates; Filters=newFilters}
            newConfig <- Map.add loc newDC newConfig
        (pred, newConfig)

    (* Make a config smaller (e.g., number of route maps) and more human-readable
       by removing community tagging information when possible *)
    let compress (cg: CGraph.T) (config: PredConfig) (outName: string) : PredConfig =
        debug1 (fun () -> System.IO.File.WriteAllText(outName + "-raw.ir", formatPrefix config))
        let config = compressUniquePairs cg config
        debug2 (fun () -> System.IO.File.WriteAllText(outName + "-min1.ir", formatPrefix config))
        (* let config = compressIsomorphicImports cg config
        debug2 (fun () -> System.IO.File.WriteAllText(outName + "-min2.ir", format config)) *)
        let config = compressIdenticalImports config
        debug2 (fun () -> System.IO.File.WriteAllText(outName + "-min3.ir", formatPrefix config))
        let config = compressTaggingWhenNotImported config
        debug2 (fun () -> System.IO.File.WriteAllText(outName + "-min4.ir", formatPrefix config))
        let config = compressAllExports cg config
        let config = compressAllImports cg config
        debug1 (fun () -> System.IO.File.WriteAllText(outName + "-min5.ir", formatPrefix config))
        config

/// Given a topology and a policy, generate a low-level configuration in an intermediate
/// byte-code-like, vendor-independent representation for BGP 

let compileToIR fullName idx pred (reb: Regex.REBuilder) res : Result<PredConfig, CounterExample> =
    let fullName = fullName + "(" + (string idx) + ")"
    (* compute the automata and product graph *)
    let dfas = 
        res 
        |> List.map (fun r -> reb.MakeDFA (Regex.rev r))
        |> Array.ofList
    let cg = CGraph.buildFromAutomata (reb.Topo()) dfas
    debug1 (fun () -> CGraph.generatePNG cg fullName )
    (* Ensure the path suffix property and dont conside simple paths *)
    CGraph.Minimize.delMissingSuffixPaths cg
    CGraph.Minimize.minimize idx cg
    (* Save graphs to file *)
    debug1 (fun () -> CGraph.generatePNG cg (fullName + "-min"))
    (* Check for errors *)
    let startingLocs = Array.fold (fun acc dfa -> Set.union (reb.StartingLocs dfa) acc) Set.empty dfas
    let originators = 
        CGraph.neighbors cg cg.Start
        |> Seq.map (fun v -> v.Node.Loc)
        |> Set.ofSeq
    let canOriginate = 
        cg.Topo.Vertices
        |> Seq.filter Topology.isInside
        |> Seq.filter Topology.canOriginateTraffic
        |> Seq.map (fun v -> v.Loc)
        |> Set.ofSeq
    let locsThatNeedPath = Set.difference (Set.intersect startingLocs canOriginate) originators
    let locsThatGetPath = CGraph.acceptingLocations cg
    logInfo1(idx, sprintf "Locations that need path: %s" (locsThatNeedPath.ToString()))
    logInfo1(idx, sprintf "Locations that get path: %s" (locsThatGetPath.ToString()))
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
            try 
                let inExports = configureIncomingTraffic cg
                match Consistency.findOrderingConservative idx cg fullName with 
                | Ok ord ->
                    let config = genConfig cg pred ord inExports
                    let config = Compress.compress cg config fullName
                    Ok (config)
                | Err((x,y)) -> Err(InconsistentPrefs(x,y))
            with 
                | UncontrollableEnterException s -> Err(UncontrollableEnter s)
                | UncontrollablePeerPreferenceException s -> Err(UncontrollablePeerPreference s)

let compileForSinglePrefix fullName idx (pred, reb, res) =
    try 
        match compileToIR fullName idx pred reb res with 
        | Ok(config) -> config
        | Err(x) -> 
            match x with
            | UnusedPreferences m ->
                error (sprintf "Unused preferences %A" m)
            | NoPathForRouters rs ->
                unimplementable (sprintf "Unable to find a path for routers: %s" (string rs))
            | InconsistentPrefs(x,y) ->
                let xs = x.ToString()
                let ys = y.ToString() 
                unimplementable (sprintf "Can not choose preference between:\n%s\n%s" xs ys)
            | UncontrollableEnter x -> 
                unimplementable (sprintf "Can not control inbound traffic from peer: %s" x)
            | UncontrollablePeerPreference x -> 
                unimplementable (sprintf "Can not control inbound preference from peer: %s without MED or prepending" x)
    with Topology.InvalidTopologyException -> 
        error (sprintf "Invalid Topology: internal topology must be weakly connected")

let checkAggregateLocs (ins: Set<string>) prefix links = 
    if ins.Contains "out" then
        error (sprintf "Cannot aggregate on external location: out for prefix: %s" (string prefix))
    match List.tryFind (fst >> Topology.isOutside) links with
    | None -> ()
    | Some x -> error (sprintf "Cannot aggregate on external location: %s for prefix: %s" (fst x).Loc (string prefix))

let getAggregates topo aggs =
    let mutable acc = Map.empty
    for (Ast.CAggregate (prefix,ins,outs)) in Seq.ofList aggs do
        let links = Topology.findLinks topo (ins,outs)
        checkAggregateLocs ins prefix links
        let pairs = 
            links
            |> List.map (fun (x,y) -> (x.Loc, y.Loc))
            |> Seq.ofList
            |> Seq.groupBy fst
            |> Seq.map (fun (x,y) -> (x, [(prefix, Seq.map snd y)]))
            |> Map.ofSeq
        acc <- Common.Map.merge acc pairs (fun _ (xs,ys) -> xs @ ys)
    acc

let compileAllPrefixes fullName topo (pairs: Ast.PolicyPair list) aggs : T =
    let aggs = getAggregates topo aggs
    Array.ofList pairs
    |> Array.Parallel.mapi (compileForSinglePrefix fullName)
    |> Array.toList
    |> joinConfigs aggs