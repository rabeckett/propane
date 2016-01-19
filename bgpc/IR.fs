module IR

open CGraph
open Common
open Common.Debug
open Common.Error
open System.Collections.Generic

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
        | PathRE r -> "Regex=" + string r
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
        | SetMed i -> "MED<-" + string i
        | PrependPath i -> "Prepend " + string i

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

type DeviceConfig =
    {Originates: bool;
     Filters: ((Import * Export list) option) list}

type PredConfig = Predicate.T * Map<string, DeviceConfig>

type DeviceAggregates = (Prefix.T list * seq<string>) list
type DeviceTags = ((string * Prefix.T list) * seq<string>) list
type DeviceMaxRoutes = (uint32 * seq<string>) list

type Aggregates = Map<string, (Prefix.T list * seq<string>) list>
type Tags = Map<string, ((string * Prefix.T list) * seq<string>) list>
type MaxRoutes = Map<string, (uint32 * seq<string>) list>

type DeviceControl = 
    {Aggregates: DeviceAggregates;
     Tags: DeviceTags;
     MaxRoutes: DeviceMaxRoutes}

type RouterConfig = 
    {Actions: (Predicate.T * DeviceConfig) list;
     Control: DeviceControl}

type T = Map<string, RouterConfig>

let joinConfigs (aggs, comms, maxroutes) (pairs: PredConfig list) : T =
    let mutable result = Map.empty
    for (prefix, config) in pairs do 
        for kv in config do
            let dc = kv.Value
            let router = kv.Key
            let value = (prefix, dc)
            match Map.tryFind router result with
            | None -> result <- Map.add router [value] result
            | Some x -> result <- Map.add router (value :: x) result
    Map.map (fun router vs ->
        let a = Common.Map.getOrDefault router [] aggs
        let b = Common.Map.getOrDefault router [] comms
        let c = Common.Map.getOrDefault router [] maxroutes
        {Actions=List.rev vs; Control={Aggregates=a; Tags=b; MaxRoutes=c}}) result

let format (config: T) = 
    let sb = System.Text.StringBuilder ()
    for kv in config do
        let routerName = kv.Key 
        let routerConfig = kv.Value
        sb.Append("\nRouter ") |> ignore
        sb.Append(routerName) |> ignore
        for (prefix, peers) in routerConfig.Control.Aggregates do
            for peer in peers do
                sb.Append (sprintf "\n  Aggregate(%s, %s)" (string prefix) peer) |> ignore
        for ((c, prefix), peers) in routerConfig.Control.Tags do 
            for peer in peers do 
                sb.Append (sprintf "\n  Tag(%s, %s, %s)" c (string prefix) peer) |> ignore
        for (i, peers)  in routerConfig.Control.MaxRoutes do 
            for peer in peers do 
                sb.Append (sprintf "\n  MaxRoutes(%d)" i) |> ignore
        for (pred, deviceConf) in routerConfig.Actions do
            let predStr = string pred
            for v in deviceConf.Filters do
                match v with 
                | None -> sb.Append("\n  No Restriction: Prefix=" + predStr) |> ignore
                | Some ((m, lp), es) ->
                    sb.Append("\n  Match: ") |> ignore
                    sb.Append("[Prefix=" + predStr + ", ") |> ignore
                    sb.Append(m.ToString() + "]") |> ignore
                    if (not deviceConf.Originates) && lp <> 100 then
                        sb.Append(" (LP=" + lp.ToString() + ")") |> ignore
                    for (peer, acts) in es do
                        sb.Append("\n    Export: [Peer<-" + peer) |> ignore
                        if acts <> [] then
                            let str = Common.List.joinBy "," (List.map string acts)
                            sb.Append(", " + str) |> ignore
                        sb.Append("]") |> ignore
        sb.Append("\n") |> ignore
    sb.ToString()

let formatPrefix pconfig = 
    format (joinConfigs (Map.empty, Map.empty, Map.empty) [pconfig])

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
        let cexample = CGraph.ToRegex.constructRegex (copyGraph cg) peer
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
            raise (UncontrollableEnterException (sprintf "(%s) incoming traffic cannot conform to: %s" p.Node.Loc (string re)))
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
let inline comparePrefThenLoc (x,i1) (y,i2) = 
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
                |> Seq.mapi (fun i v -> Seq.map (fun n -> (n,i)) (neighborsIn cg v))
                |> Seq.fold Seq.append Seq.empty
                |> Seq.toArray 
                |> Array.sortWith comparePrefThenLoc
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
                    |> Seq.find (fun x -> x.Node.Loc = loc)
                let expNeighbors = 
                    neighbors cg node
                    |> Seq.toArray
                    |> Array.filter (fun x -> x.Node.Loc <> v.Node.Loc && Topology.isTopoNode x.Node)
                
                (* Set exports *)
                let mutable exports = Map.empty
                for n in expNeighbors do
                    let loc = n.Node.Loc
                    if Topology.isOutside n.Node then
                        exports <- Map.add loc (Map.find n inExports) exports
                    else exports <- Map.add loc [SetComm(string (commMapper node.States))] exports

                filters <- Some ((m,lp), Map.toList exports) :: filters
                originates <- v.Node.Typ = Topology.Start

            let deviceConf = {Originates=originates; Filters=filters}
            config <- Map.add loc deviceConf config
    (pred, config)


module Compress =

    let updateDC ((pred, config): PredConfig) f =
        let nConfig = 
            Map.fold (fun acc router dc ->
                let filters = List.choose (f router) dc.Filters
                let dc' = {Originates=dc.Originates; Filters=filters}
                Map.add router dc' acc ) Map.empty config
        (pred,nConfig)

    let updateExports (es: Export list) f : Export list = 
        List.choose f es

    let inline isCommunityTag (action: Action) : bool = 
        match action with
        | SetComm _ -> true
        | _ -> false

    let inline isCommunity is (action: Action) : bool = 
        match action with
        | SetComm is' -> is = is'
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
    let compressUniquePairs (cg: CGraph.T) (config: PredConfig) : PredConfig =
        let pairCount =
            cg.Graph.Edges
            |> Seq.countBy (fun e -> (e.Source.Node.Loc, e.Target.Node.Loc))
        let pCount = Dictionary()
        for e, count in pairCount do
            pCount.[e] <- count
        let inline unqPair e = pCount.[e] = 1 
        updateDC config (fun loc v ->
            let (Some ((m,lp),es)) = v 
            let m' = 
                match m with
                | Match.State(is,x) -> if unqPair (x,loc) then Match.Peer(x) else m
                | _ -> m
            let es' = updateExports es (fun (peer, acts) -> 
                let acts' =
                    match getTag acts with
                    | None -> acts
                    | Some is -> if unqPair (loc,peer) then List.filter (isCommunityTag >> not) acts else acts
                Some (peer, acts'))
            Some (Some ((m', lp), es')))

    let compressRedundantTagging (cg: CGraph.T) (config: PredConfig) : PredConfig = 
        updateDC config (fun router filter -> 
            let (Some ((m,lp),es)) = filter
            match m with
            | Match.State(is,p) -> 
                let es' = updateExports es (fun (peer, acts) -> 
                    Some (peer, List.filter (isCommunity is >> not) acts))
                Some (Some ((m,lp), es'))
            | _ -> Some filter)

    let compressNoRestriction (cg: CGraph.T) (config: PredConfig) : PredConfig = 
        updateDC config (fun router filter -> 
            let (Some ((m,lp),es)) = filter
            match m, es with
            | Match.Peer "*", [("*",_)] ->
                Some None 
            | _ -> Some filter)

    (*
    (* Find the longest consecutive sequence of imports for each peer. 
       Only imports where the community is matched are counted towards the sequence.
       This makes a single pass over the list to find the largest sequence for each peer *)
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
            | [x] -> Reachable.supersetPaths -1 (cg, x) (cg, fst)
            | x::((y::z) as tl) -> (Reachable.supersetPaths -1 (cg, x) (cg, y)) && (aux fst (y::z))
        match nodes with
        | [] -> true
        | [x] -> true
        | x::y::z -> aux x nodes

    let removeCommFiltersForStates (states: (string * int array) list) (dc: DeviceConfig) : DeviceConfig =
        let relevant x = 
            List.exists ((=) x) states
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
    let compressIsomorphicImports (cg: CGraph.T) ((prefix, config): PredConfig) : PredConfig =
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
            | (Some ((m,lp), es) as f)::fs ->
                let cleaned = List.filter (fun (Some ((m',_),_)) -> m' <> m) fs
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
        let importCommNeeded = Dictionary()
        for kv in config do 
            let loc = kv.Key 
            let deviceConf = kv.Value 
            for v in deviceConf.Filters do
                let (Some ((m,_), _)) = v
                match m with 
                | Match.State(is,x) ->
                    let edge = (loc,x)
                    if importCommNeeded.ContainsKey edge then
                        let iss = importCommNeeded.[edge]
                        importCommNeeded.[edge] <- Set.add is iss
                    else importCommNeeded.[edge] <- (Set.singleton is)
                | _ -> ()
        let newConfig = 
            Map.map (fun loc dc ->
                let filters' = 
                    List.map (fun (Some ((m,lp), es)) ->
                        let es' = 
                            List.map (fun (peer,acts) ->
                                let acts' = 
                                    List.filter (fun a ->
                                        match a with
                                        | SetComm is ->
                                            let edge = (peer,loc)
                                            importCommNeeded.ContainsKey edge &&
                                            importCommNeeded.[edge].Contains is
                                        | _ -> true
                                    ) acts
                                (peer,acts')
                            ) es
                        Some ((m,lp), es')
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
            let topoPeers = 
                cg.Topo.OutEdges topoNode
                |> Seq.map (fun e -> e.Target.Loc)
                |> Set.ofSeq
            for f in filters do 
                let (Some ((m,lp), es)) = f 
                let eqActions =
                    es
                    |> List.map snd 
                    |> Set.ofList
                    |> Set.count
                    |> ((=) 1)
                let exports = Set.ofList (List.map fst es)
                let exports = 
                    match m with 
                    | Match.Peer(x) -> Set.add x exports
                    | Match.State(_,x) -> Set.add x exports
                    | _ -> exports
                let allPeers = (Set.isEmpty (Set.difference topoPeers exports))
                if eqActions && allPeers && List.length es <= Seq.length topoPeers then 
                    newFilters <- Some ((m,lp), [("*", snd es.Head)]) :: newFilters
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
                |> List.map (Option.get >> snd) 
                |> Set.ofList
                |> Set.count
                |> ((=) 1)
            let eqLP = 
                filters
                |> List.map (Option.get >> fst >> snd)
                |> Set.ofList
                |> Set.count
                |> ((=) 1)
            let noComms = 
                filters
                |> List.map (Option.get >> fst >> fst)
                |> List.forall (fun x -> match x with Match.Peer _ -> true | _ -> false)
            let coversPeer p = 
                List.exists (fun  (Some ((m,lp),es)) -> 
                    match m with
                    | Match.Peer(x) -> x = p
                    | _ -> false
                ) filters
            let allPeers = Seq.forall coversPeer topoPeers
            let allSame = eqActions && eqLP && noComms && allPeers
            let newFilters = 
                if allSame && List.length filters = Seq.length topoPeers && not (List.isEmpty filters)
                then
                    let (Some ((m,lp),es)) = List.head filters 
                    [Some ((Match.Peer("*"), lp), es)]
                else filters
            let newDC = {Originates=deviceConf.Originates; Filters=newFilters}
            newConfig <- Map.add loc newDC newConfig
        (pred, newConfig)

    (* Make a config smaller (e.g., number of route maps) and more human-readable
       by removing community tagging information when possible *)
    let compress (cg: CGraph.T) (config: PredConfig) (outName: string) : PredConfig =
        let config = compressUniquePairs cg config
        // let config = compressIsomorphicImports cg config
        let config = compressIdenticalImports config
        let config = compressTaggingWhenNotImported config
        let config = compressAllExports cg config
        let config = compressAllImports cg config
        let config = compressRedundantTagging cg config
        let config = compressNoRestriction cg config
        config

/// Given a topology and a policy, generate a low-level configuration in an intermediate
/// byte-code-like, vendor-independent representation for BGP 

let getLocsThatCantGetPath idx cg (reb: Regex.REBuilder) dfas = 
    let startingLocs = Array.fold (fun acc dfa -> Set.union (reb.StartingLocs dfa) acc) Set.empty dfas
    let originators = 
        CGraph.neighbors cg cg.Start
        |> Seq.map (fun v -> v.Node.Loc)
        |> Set.ofSeq
    let canOriginate = 
        cg.Topo.Vertices
        |> Seq.filter (fun v -> Topology.isInside v && Topology.canOriginateTraffic v)
        |> Seq.map (fun v -> v.Loc)
        |> Set.ofSeq
    let locsThatNeedPath = Set.difference (Set.intersect startingLocs canOriginate) originators
    let locsThatGetPath = CGraph.acceptingLocations cg
    logInfo1(idx, sprintf "Locations that need path: %s" (locsThatNeedPath.ToString()))
    logInfo1(idx, sprintf "Locations that get path: %s" (locsThatGetPath.ToString()))
    Set.difference locsThatNeedPath locsThatGetPath

let getUnusedPrefs cg res = 
    let numberedRegexes = seq {for i in 1.. List.length res do yield i}  |> Set.ofSeq
    let prefs = CGraph.preferences cg
    Set.difference numberedRegexes prefs

let compileToIR fullName idx pred (reb: Regex.REBuilder) res : Result<PredConfig, CounterExample> =
    let settings = Args.getSettings ()
    let fullName = fullName + "(" + (string idx) + ")"
    let dfas = List.map (fun r -> reb.MakeDFA (Regex.rev r)) res |> Array.ofList
    let cg = CGraph.buildFromAutomata (reb.Topo()) dfas
    debug1 (fun () -> CGraph.generatePNG cg fullName )
    let cg = CGraph.Minimize.delMissingSuffixPaths cg
    let cg = CGraph.Minimize.minimize idx cg
    debug1 (fun () -> CGraph.generatePNG cg (fullName + "-min"))
    let lost = getLocsThatCantGetPath idx cg reb dfas
    if not (Set.isEmpty lost) then Err(NoPathForRouters(lost)) else
    let unusedPrefs = getUnusedPrefs cg res
    if not (Set.isEmpty unusedPrefs) then
        let cexamples = Set.fold (fun acc p -> 
            Map.add p (List.item (p-1) res) acc) Map.empty unusedPrefs
        Err(UnusedPreferences(cexamples))
    else
        try 
            let inExports = configureIncomingTraffic cg
            match Consistency.findOrderingConservative idx cg fullName with 
            | Ok ord ->
                let config = genConfig cg pred ord inExports
                if settings.Compression then 
                    let compressed = Compress.compress cg config fullName
                    Ok (compressed)
                else Ok (config)
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

let checkAggregateLocs ins _ prefix links = 
    if Set.contains "out" ins then
        error (sprintf "Cannot aggregate on external location: out for prefix: %s" (string prefix))
    match List.tryFind (fst >> Topology.isOutside) links with
    | None -> ()
    | Some x -> 
        error (sprintf "Cannot aggregate on external location: %s for prefix: %s" (fst x).Loc (string prefix))

let checkCommunityTagLocs ins _ (c, prefix) links =
    if Set.contains "out" ins then
        error (sprintf "Cannot tag communities on external location: out for community %s, prefix: %s" c (string prefix))
    match List.tryFind (fst >> Topology.isOutside) links with
    | None -> ()
    | Some x -> 
        error (sprintf "Cannot tag communities on external location: %s for community %s prefix: %s" (fst x).Loc c (string prefix))

let checkMaxRouteLocs ins outs i links =
    let v = List.exists (fun (a,b) -> Topology.isOutside a && Topology.isOutside b) links
    let w = Set.contains "out" ins
    let x = Set.contains "out" outs
    let y = List.exists (fst >> Topology.isOutside) links
    let z = List.exists (snd >> Topology.isOutside) links
    if v || ((w || y) && (x || z)) then 
        error (sprintf "cannot set maxroutes(%d) on links without and edge in the internal topology" i)

let splitByLocation f topo (vs: _ list) = 
    let mutable acc = Map.empty
    for (k, ins, outs) in vs do 
        let links = Topology.findLinks topo (ins,outs)
        f ins outs k links
        let pairs = 
            links
            |> List.map (fun (x,y) -> (x.Loc, y.Loc))
            |> Seq.ofList
            |> Seq.groupBy fst
            |> Seq.map (fun (x,y) -> (x, [(k, Seq.map snd y)]))
            |> Map.ofSeq
        acc <- Common.Map.merge acc pairs (fun _ (xs,ys) -> xs @ ys)
    acc

let splitConstraints topo (constraints: _ list) =
    let aggs, comms, maxroutes = 
        List.fold (fun ((x,y,z) as acc) c -> 
            match c with
            | Ast.CAggregate (p,ins,outs) -> ((p,ins,outs)::x, y, z)
            | Ast.CCommunity (s,p,ins,outs) -> (x, ((s,p),ins,outs)::y, z)
            | Ast.CMaxRoutes (i,ins,outs) -> (x, y, (i,ins,outs)::z)
            | _ -> acc
        ) ([], [], []) constraints
    let aggInfo = splitByLocation checkAggregateLocs topo aggs
    let commInfo = splitByLocation checkCommunityTagLocs topo comms
    let maxRouteInfo = splitByLocation checkMaxRouteLocs topo maxroutes
    (aggInfo, commInfo, maxRouteInfo)
    
type Stats = 
    {TotalTime: int64;
     NumPrefixes: int;
     PrefixTime: int64;
     PerPrefixTimes: int64 array
     JoinTime: int64;}

let compileAllPrefixes fullName topo (pairs: Ast.PolicyPair list) constraints : T * Stats =
    let info = splitConstraints topo constraints
    let pairs = Array.ofList pairs
    let timedConfigs, prefixTime =
        Profile.time (Array.Parallel.mapi (fun i x -> 
            Profile.time (compileForSinglePrefix fullName i) x)) pairs
    let configs, times = Array.unzip timedConfigs
    let joined, joinTime = Profile.time (joinConfigs info) (Array.toList configs)
    let stats = 
        {TotalTime=prefixTime + joinTime;
         PrefixTime=prefixTime;
         PerPrefixTimes=times
         JoinTime=joinTime; 
         NumPrefixes=Array.length configs;}
    joined, stats