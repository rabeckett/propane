module IR

open CGraph
open Common
open Common.Debug
open Common.Error
open Common.Format
open System.Collections.Generic

exception UncontrollableEnterException of string
exception UncontrollablePeerPreferenceException of string

type CounterExample = 
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
    | SetComm of string
    | SetMed of int
    | PrependPath of int

    override this.ToString() = 
        match this with 
        | SetComm(is) -> "Community<-" + is
        | SetMed i -> "MED<-" + string i
        | PrependPath i -> "Prepend " + string i

type LocalPref = int
type Peer = string
type Import = Match * LocalPref
type Export = Peer * Action list

/// Result from compiling a single prefix

type Filter = 
    | Deny
    | Allow of Import * (Export list)

type DeviceConfig =
    {Originates: bool;
     Filters: Filter list}

type PredConfig = Predicate.T * Map<string, DeviceConfig>

type AggregationSafetyResult = (int * string * string * Prefix.T * Prefix.T) option

type PrefixResult =
    {K: AggregationSafetyResult;
     BuildTime: int64;
     MinimizeTime: int64;
     OrderingTime: int64;
     ConfigTime: int64;
     CompressSizeInit: int;
     CompressSizeFinal: int;
     Config: PredConfig}

type CompileResult = Result<PrefixResult, CounterExample>

/// Result from compiling the entire policy

type DeviceAggregates = (Prefix.T * seq<string>) list
type DeviceTags = ((string * Prefix.T list) * seq<string>) list
type DeviceMaxRoutes = (uint32 * seq<string>) list

type DeviceControl = 
    {Aggregates: DeviceAggregates;
     Tags: DeviceTags;
     MaxRoutes: DeviceMaxRoutes}

type RouterConfig = 
    {Actions: (Predicate.T * DeviceConfig) list;
     Control: DeviceControl}

type T = Map<string, RouterConfig>

let joinConfigs (aggs, comms, maxroutes) (results: PrefixResult list) : T =
    let mutable result = Map.empty
    for v in results do
        let (prefix, config) = v.Config 
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
            for f in deviceConf.Filters do
                match f with 
                | Deny ->
                    sb.Append("\n  Deny: ") |> ignore
                    sb.Append("[Pred=" + predStr + "]") |> ignore
                | Allow ((m, lp), es) ->
                    match m with 
                    | NoMatch -> sb.Append ("\n  Originate: [Pred=" + predStr + "]") |> ignore
                    | _ ->
                        sb.Append("\n  Allow: ") |> ignore
                        sb.Append("[Pred=" + predStr + ", " + string m + "]") |> ignore
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
    // Temporary 
    if not settings.CheckEnter then 
        let mutable exportMap = Map.empty 
        for p in info.Peers do 
            exportMap <- Map.add p [] exportMap
        exportMap
    else
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


(* Generate a configuration from the product graph *)

type OutPeerMatch = 
    | PeerMatch of CgState
    | RegexMatch of Regex.T

let inline isPeerMatch x = 
    match x with 
    | PeerMatch _ -> true
    | RegexMatch _ -> false

let inline getPeerMatch x = 
    match x with 
    | PeerMatch y -> y
    | RegexMatch _ -> failwith "unreachable"

let inline getRegexMatch x = 
    match x with 
    | PeerMatch _ -> failwith "unreachable"
    | RegexMatch y -> y

let getOutPeerType cg (x:CgState) = 
    if Topology.isOutside x.Node then
        let nin = neighborsIn cg x
        if Seq.length nin = 2 &&
           Seq.exists ((=) cg.Start) nin && 
           Seq.exists (fun v -> 
                isRepeatedOut cg v && 
                let nin = neighborsIn cg v 
                Seq.length nin = 2 && 
                Seq.exists ((=) cg.Start) nin) nin then
            PeerMatch x
        else RegexMatch (CGraph.ToRegex.constructRegex (CGraph.copyReverseGraph cg) x)
    else PeerMatch x

let getMatches (allPeers, inPeers, outPeers) incomingMatches =
    let inline eqStates states = 
        Set.count states = 1
    let peermatches, regexes = List.partition isPeerMatch (List.ofSeq incomingMatches)
    
    let mutable peers = Set.ofList (List.map getPeerMatch peermatches)
    let peerLocs = Set.map CGraph.loc peers
    let peersIn = Set.filter (fun v -> Topology.isInside v.Node) peers
    let peersOut = Set.filter (fun v -> Topology.isOutside v.Node) peers
    let peerLocsIn =  Set.map CGraph.loc peersIn
    let peerLocsOut = Set.map CGraph.loc peersOut

    let mutable matches = 
        List.map (fun r -> Match.PathRE (getRegexMatch r)) regexes
    let states = peers |> Set.map (fun v -> v.State)
    let statesIn = peers |> Set.filter (fun v -> Topology.isInside v.Node) |> Set.map (fun v -> v.State)
    let statesOut = peers |> Set.filter (fun v -> Topology.isOutside v.Node) |> Set.map (fun v -> v.State)
    if peerLocs = allPeers && (eqStates states) then 
        matches <- Match.State(string (Set.minElement states),"*") :: matches
        peers <- Set.empty
    else 
        if peerLocsIn = inPeers && (eqStates statesIn) && not peerLocsIn.IsEmpty then 
            matches <- Match.State(string (Set.minElement statesIn),"in") :: matches
            peers <- Set.difference peers peersIn
        if peerLocsOut = outPeers && (eqStates statesOut) && not peerLocsOut.IsEmpty then 
            matches <- Match.Peer("out") :: matches
            peers <- Set.difference peers peersOut
        for v in peers do
            let m = 
                if Topology.isInside v.Node then  
                    Match.State(string v.State, v.Node.Loc)
                else Match.Peer(v.Node.Loc)
            matches <- m :: matches

    matches

let inline getExport specialCase inExports x v = 
    if Topology.isOutside v.Node 
    then 
        let ret = Map.find v inExports
        if ret <> [] then 
            specialCase := true
        ret
    else [SetComm(string x.State)]

let getExports (allPeers, inPeers, outPeers) x inExports (outgoing: seq<CgState>) unqMatchPeer = 
    let toInside, toOutside = 
        List.ofSeq outgoing 
        |> List.partition (fun v -> Topology.isInside v.Node)
    let insideExport = [("in", [SetComm (string x.State)])]
    let specialCase = ref false
    let exports = List.ofSeq toOutside |> List.map (fun v -> (v.Node.Loc, getExport specialCase inExports x v)) 
    if not !specialCase then
        let sendToLocs = Seq.map (fun v -> v.Node.Loc) outgoing |> Set.ofSeq
        let sendToLocs = 
            match unqMatchPeer with
            | Some x -> Set.add x sendToLocs
            | None -> sendToLocs
        if sendToLocs.IsSupersetOf outPeers then
            [("*", [SetComm(string x.State)])]
        else exports @ insideExport
    else  exports @ insideExport

let updateExports (es: Export list) f : Export list = 
    List.choose f es

let removeRedundantTag exports m = 
    match m with 
    | Match.Peer _ | Match.PathRE _ | Match.NoMatch -> exports 
    | Match.State(is,_) -> 
        updateExports exports (fun (peer, acts) -> 
            let acts' = List.filter (fun a -> 
                match a with 
                | SetComm c -> c <> is
                | _ -> true) acts
            Some (peer, acts') )

let removeCommMatch cg eCounts v m = 
    let inline unq e = 
        match Map.tryFind e eCounts with 
        | Some 1 -> true
        | _ -> false
    match m with 
    | Match.State (c,peers) -> 
        if peers = "*" then 
            let ins = CGraph.neighborsIn cg v |> Seq.map CGraph.loc
            if Seq.forall (fun i -> unq (i, v.Node.Loc)) ins then Match.Peer(peers) else m 
        else if unq (peers, v.Node.Loc) then Match.Peer(peers)
        else m
    | _ -> m

let edgeCounts (cg: CGraph.T) =
    cg.Graph.Edges
    |> Seq.fold (fun acc e -> 
            let key = (e.Source.Node.Loc, e.Target.Node.Loc)
            Common.Map.adjust key 0 ((+) 1) acc ) Map.empty

let getPeerInfo vs =
    let inline setLocs x = 
        Set.ofSeq (Seq.map (fun (v: Topology.State) -> v.Loc) x) 
    let all = vs |> setLocs
    let allIn = vs |> Seq.filter Topology.isInside |> setLocs
    let allOut = vs |> Seq.filter Topology.isOutside |> setLocs
    (all, allIn, allOut)

let genConfig (cg: CGraph.T) (pred: Predicate.T) (ord: Consistency.Ordering) (inExports: IncomingExportMap) : PredConfig * int * int =
    let settings = Args.getSettings ()
    let ain = Topology.alphabet cg.Topo |> fst |> Set.map (fun v -> v.Loc)
    let eCounts = edgeCounts cg
    let szRaw = ref 0 
    let szSmart = ref 0
    let mutable config = Map.empty
    // generate a config for each internal router
    for router in ain do
        let mutable filters = []
        let mutable originates = false
        // look at the nodes according to preference
        match Map.tryFind router ord with 
        | None -> ()
        | Some prefs ->
            let mutable rules = []
            let mutable lp = 101
            for cgstate in prefs do
                lp <- lp - 1
                // get incoming and outgoing topology peer information
                let allInPeers = cg.Topo.InEdges cgstate.Node |> Seq.map (fun e -> e.Source)
                let allOutPeers = cg.Topo.OutEdges cgstate.Node |> Seq.map (fun e -> e.Target)
                let inPeerInfo = getPeerInfo allInPeers
                let outPeerInfo = getPeerInfo allOutPeers
                let nsIn = neighborsIn cg cgstate
                let nsOut = neighbors cg cgstate
                // find who this node needs to send to and receive from
                let receiveFrom = Seq.filter CGraph.isRealNode nsIn
                let sendTo = Seq.filter CGraph.isRealNode nsOut
                // helps to minimize configuration
                let peerTypes = Seq.map (getOutPeerType cg) receiveFrom
                let origin = Seq.exists ((=) cg.Start) nsIn
                // get the compressed set of matches using *, in, out when possible
                let matches =  if origin then [Match.NoMatch] else getMatches inPeerInfo peerTypes
                // get the compressed set of exports taking into account if there is a unique receive peer
                let unqMatchPeer = 
                    match matches with 
                    | [Match.Peer x] -> Some x
                    | [Match.State(_,x)] -> Some x 
                    | _ -> None
                let exports = getExports outPeerInfo cgstate inExports sendTo unqMatchPeer 
                // perform community minimization while adding the match export filters
                for m in matches do 
                    let exports = removeRedundantTag exports m
                    let m = removeCommMatch cg eCounts cgstate m
                    filters <- Allow ((m,lp), exports) :: filters
                originates <- origin || originates
                // update the compression stats
                szRaw := !szRaw + (Seq.length nsIn) * (Seq.length nsOut)
                szSmart := !szSmart + (List.length exports)
        szSmart := !szSmart + (List.length filters)
        // no need for explicit deny if we allow everything
        match filters with 
        | [Allow ((Match.Peer "*",_), _)] -> ()
        | [Allow ((Match.NoMatch,_), _)] -> ()
        | _ -> filters <- List.rev (Deny :: filters)
        // build the final configuration
        let deviceConf = {Originates=originates; Filters=filters}
        config <- Map.add router deviceConf config
    (pred, config), !szRaw, !szSmart

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
    |> Set.filter (fun i -> res.[i-1] <> Regex.empty)

// TODO: only do this once total per prefix
let getMinAggregateFailures (cg: CGraph.T) pred (aggInfo: Map<string, DeviceAggregates>) = 
    let originators = CGraph.neighbors cg cg.Start
    let prefixes = Predicate.getPrefixes pred
    let mutable smallest = System.Int32.MaxValue
    let mutable pairs = None
    for p in prefixes do
        Map.iter (fun aggRouter aggs ->
            let relevantAggs = List.filter (fun (prefix, _) -> Prefix.implies (Prefix.toPredicate [prefix]) p) aggs
            if not relevantAggs.IsEmpty then 
                let rAgg, _ = relevantAggs.Head
                match CGraph.Failure.disconnectLocs cg originators aggRouter with 
                | None -> () 
                | Some (k,x,y) ->
                    if k < smallest then 
                        smallest <- min smallest k
                        let p = (x,y, List.head (Prefix.toPrefixes p), rAgg)
                        pairs <- Some p ) aggInfo
    if smallest = System.Int32.MaxValue then None
    else let x,y,p,agg = Option.get pairs in Some (smallest, x, y, p, agg)

let compileToIR fullName idx pred (aggInfo: Map<string,DeviceAggregates>) (reb: Regex.REBuilder) res : CompileResult =
    let settings = Args.getSettings ()
    let fullName = fullName + "(" + (string idx) + ")"
    let dfas, dfaTime = Profile.time (List.map (fun r -> reb.MakeDFA (Regex.rev r))) res
    let dfas = Array.ofList dfas
    let cg, pgTime = Profile.time (CGraph.buildFromAutomata (reb.Topo())) dfas
    let buildTime = dfaTime + pgTime
    debug1 (fun () -> CGraph.generatePNG cg fullName )
    let cg, delTime = Profile.time (CGraph.Minimize.delMissingSuffixPaths) cg
    let cg, minTime = Profile.time (CGraph.Minimize.minimize idx) cg
    let minTime = delTime + minTime
    debug1 (fun () -> CGraph.generatePNG cg (fullName + "-min")) 
    // check there is a route for each location specified
    let lost = getLocsThatCantGetPath idx cg reb dfas
    if not (Set.isEmpty lost) then Err(NoPathForRouters(lost)) else
    // Find unused preferences for policies that were not drop
    let unusedPrefs = getUnusedPrefs cg res
    if not (Set.isEmpty unusedPrefs)  then
        for i in unusedPrefs do 
            warning (sprintf "Unused preference %d for predicate %s for a non-drop policy" i (string pred))
    try
        // check that BGP can ensure incoming traffic compliance
        let inExports = configureIncomingTraffic cg
        // check aggregation failure consistency
        let k = getMinAggregateFailures cg pred aggInfo
        // check  that BGP preferences can be set properly
        let (ordering, orderTime) = Profile.time (Consistency.findOrderingConservative idx cg) fullName
        match ordering with 
        | Ok ord ->
            let (config, szRaw, szSmart), configTime = Profile.time (genConfig cg pred ord) inExports
            let result = 
                {K=k; 
                  BuildTime=buildTime; 
                  MinimizeTime=minTime; 
                  OrderingTime=orderTime;
                  ConfigTime=configTime; 
                  CompressSizeInit=szRaw;
                  CompressSizeFinal=szSmart;
                  Config=config}
            debug1 (fun () -> System.IO.File.WriteAllText (sprintf "%s.ir" fullName, formatPrefix result) )
            Ok (result)
        | Err((x,y)) -> Err(InconsistentPrefs(x,y))
    with 
        | UncontrollableEnterException s -> Err(UncontrollableEnter s)
        | UncontrollablePeerPreferenceException s -> Err(UncontrollablePeerPreference s)

let compileForSinglePrefix fullName idx (aggInfo: Map<string, DeviceAggregates>) (pred, reb, res) =
    match compileToIR fullName idx pred aggInfo reb res with 
    | Ok(config) -> config
    | Err(x) ->
        match x with
        | NoPathForRouters rs ->
            error (sprintf "Unable to find a path for routers: %s for predicate %s" (string rs) (string pred))
        | InconsistentPrefs(x,y) ->
            let msg = sprintf "Cannot find preferences for router %s for predicate %s" x.Node.Loc (string pred)
            error msg
        | UncontrollableEnter x -> 
            let msg = sprintf "Cannot control inbound traffic from peer: %s for predicate %s" x (string pred)
            error msg
        | UncontrollablePeerPreference x -> 
            let msg = sprintf "Cannot control inbound preference from peer: %s for predicate %s. Possibly enable prepending: --prepending:on" x (string pred)
            error msg

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
        error (sprintf "Cannot set maxroutes(%d) on links without an edge in the internal topology" i)

let splitByLocation f topo (vs: _ list) = 
    let mutable acc = Map.empty
    for (k, ins, outs) in vs do 
        let links = Topology.findLinks topo (ins,outs)
        f ins outs k links
        let pairs = 
            links
            |> List.map (fun (x,y) -> (x.Loc, y.Loc))
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
    {NumPrefixes: int;
     SizeRaw: int;
     SizeCompressed: int;
     TotalTime: int64;
     PrefixTime: int64;
     PerPrefixTimes: int64 array;
     PerPrefixBuildTimes: int64 array;
     PerPrefixMinTimes: int64 array;
     PerPrefixOrderTimes: int64 array;
     PerPrefixGenTimes: int64 array;
     JoinTime: int64;}

let minFails x y = 
    match x, y with 
    | None, _ -> y 
    | _, None -> x 
    | Some (i,_,_,_,_), Some (j,_,_,_,_) -> 
        if i < j then x else y

let compileAllPrefixes fullName topo (pairs: Ast.PolicyPair list) constraints : T * AggregationSafetyResult * Stats =
    let settings = Args.getSettings () 
    let mapi = if settings.Parallel then Array.Parallel.mapi else Array.mapi
    let info = splitConstraints topo constraints
    let (aggInfo, _, _) = info
    let pairs = Array.ofList pairs
    let timedConfigs, prefixTime =
        Profile.time (mapi (fun i x -> 
            Profile.time (compileForSinglePrefix fullName (i+1) aggInfo) x)) pairs
    let nAggFails = Array.map (fun (res,_) -> res.K) timedConfigs
    let k = Array.fold minFails None nAggFails
    let configs, times = Array.unzip timedConfigs
    let joined, joinTime = Profile.time (joinConfigs info) (Array.toList configs)    
    let buildTimes = Array.map (fun c -> c.BuildTime) configs
    let minTimes = Array.map (fun c -> c.MinimizeTime) configs
    let orderTimes = Array.map (fun c -> c.OrderingTime) configs
    let genTimes = Array.map (fun c -> c.ConfigTime) configs
    let szInit = Array.fold (fun acc c -> c.CompressSizeInit + acc) 0 configs
    let szFinal = Array.fold (fun acc c -> c.CompressSizeFinal + acc) 0 configs
    let stats = 
        {NumPrefixes=Array.length configs;
         SizeRaw=szInit;
         SizeCompressed=szFinal;
         TotalTime=prefixTime + joinTime;
         PrefixTime=prefixTime;
         PerPrefixTimes=times
         PerPrefixBuildTimes=buildTimes;
         PerPrefixMinTimes=minTimes;
         PerPrefixOrderTimes=orderTimes;
         PerPrefixGenTimes=genTimes;
         JoinTime=joinTime}
    joined, k, stats



module Test = 

    let isPeer ain x m = 
        match m with 
        | Peer y -> x = y || y = "*" || (y = "in" && Set.contains x ain)
        | State(_,y) -> x = y || y = "*" || (y = "in" && Set.contains x ain)
        | _ -> false

    let getPref ain (x:string) (dc: DeviceConfig) = 
        let lp = List.tryFind (fun f -> 
            match f with 
            | Deny -> false 
            | Allow ((m,_),_) -> isPeer ain x m) dc.Filters
        match lp with
        | Some (Allow ((_,lp), _)) -> lp
        | _ -> 100

    let prefersPeer ain (_,config) x (a,b) =
        try 
            let deviceConfig = Map.find x config 
            let lp1 = getPref ain a deviceConfig
            let lp2 = getPref ain b deviceConfig
            lp1 > lp2
        with _ -> false

    let receiveFrom ain (_,config) x y = 
        let deviceConf = Map.find x config 
        List.exists (fun f -> 
            match f with 
            | Deny -> false 
            | Allow ((m,_), _) -> isPeer ain y m) deviceConf.Filters

    let originates ((_, config): PredConfig) x =
        let deviceConfig = Map.find x config
        deviceConfig.Originates

    type FailReason = 
        | FRInconsistentPrefs
        | FRNoPathForRouters
        | FRCantControlPeers

    type Test = 
        {Name: string;
         Explanation: string;
         Topo: Topology.T;
         Rf: Regex.REBuilder -> Regex.T list;
         Receive: (string*string) list option;
         Originate: string list option;
         Prefs: (string*string*string) list option
         Fail: FailReason option}

    let tDiamond = Topology.Examples.topoDiamond () 
    let tDatacenterSmall = Topology.Examples.topoDatacenterSmall ()
    let tDatacenterMedium = Topology.Examples.topoDatacenterMedium ()
    let tDatacenterLarge = Topology.Examples.topoDatacenterLarge ()
    let tBrokenTriangle = Topology.Examples.topoBrokenTriangle ()
    let tBigDipper = Topology.Examples.topoBigDipper () 
    let tBadGadget = Topology.Examples.topoBadGadget ()
    let tSeesaw = Topology.Examples.topoSeesaw ()
    let tStretchingManWAN = Topology.Examples.topoStretchingManWAN ()
    let tStretchingManWAN2 = Topology.Examples.topoStretchingManWAN2 ()
    let tPinCushionWAN = Topology.Examples.topoPinCushionWAN ()
    let tBackboneWAN = Topology.Examples.topoBackboneWAN ()

    let rDiamond1 (reb: Regex.REBuilder) = 
        let pref1 = reb.Concat (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])
        [reb.Build pref1]

    let rDiamond2 (reb: Regex.REBuilder) = 
        let pref1 = reb.Concat (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])
        let pref2 = reb.Concat [reb.Loc "A"; reb.Star reb.Inside; reb.Loc "N"; reb.Loc "Z"; reb.Loc "B"]
        [reb.Build pref1; reb.Build pref2]

    let rDatacenterSmall1 (reb: Regex.REBuilder) = 
        let pref1 = reb.Internal()
        [reb.Build pref1]

    let rDatacenterSmall2 (reb: Regex.REBuilder) = 
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        [reb.Build pref1]

    let rDatacenterSmall3 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        let pref2 = reb.Inter [reb.Internal(); reb.End ["A"]]
        [reb.Build pref1; reb.Build pref2]

    let rDatacenterSmall4 (reb: Regex.REBuilder) =
        let pref1 = reb.End(["A"])
        [reb.Build pref1]

    let rDatacenterSmall5 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        let pref2 = reb.Inter [reb.Through ["N"]; reb.End ["A"]]
        let pref3 = reb.Inter [reb.End ["A"]]
        [reb.Build pref1; reb.Build pref2; reb.Build pref3]

    let rDatacenterMedium1 (reb: Regex.REBuilder) =
        let pref1 = reb.Internal()
        [reb.Build pref1]

    let rDatacenterMedium2 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Start ["A"]; reb.Through ["X"]; reb.End ["F"]]
        [reb.Build pref1]

    let rDatacenterMedium3 (reb: Regex.REBuilder) =
        let vf = reb.ValleyFree([["A";"B";"E";"F"]; ["C";"D";"G";"H"]; ["X";"Y"]])
        let pref1 = reb.Inter [reb.Through ["X"]; reb.End ["F"]; vf]
        [reb.Build pref1]

    let rDatacenterMedium4 (reb: Regex.REBuilder) =
        let vf = reb.ValleyFree([["A";"B";"E";"F"]; ["C";"D";"G";"H"]; ["X";"Y"]])
        let start = reb.Start(["A"; "B"])
        let pref1 = reb.Inter [start; reb.Through ["X"]; reb.End ["F"]; vf]
        let pref2 = reb.Inter [reb.End ["F"]; vf]
        [reb.Build pref1; reb.Build pref2]

    let rDatacenterMedium5 (reb: Regex.REBuilder) =
        let vf = reb.ValleyFree([["A";"B";"E";"F"]; ["C";"D";"G";"H"]; ["X";"Y"]])
        let pref1 = reb.Inter [reb.Through ["X"]; reb.End ["F"]; vf]
        let pref2 = reb.Inter [reb.Through ["Y"]; reb.End ["F"]; vf]
        [reb.Build pref1; reb.Build pref2]

    let rDatacenterMedium6 (reb: Regex.REBuilder) =
        let vf = reb.ValleyFree([["A";"B";"E";"F"]; ["C";"D";"G";"H"]; ["X";"Y"]])
        let pref1 = reb.Inter [reb.Through ["X"]; reb.End ["F"]; vf]
        let pref2 = reb.Inter [reb.Through ["Y"]; reb.End ["F"]; vf]
        let pref3 = reb.Inter [reb.End ["F"]; vf]
        [reb.Build pref1; reb.Build pref2; reb.Build pref3]

    let rDatacenterLarge1 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        [reb.Build pref1]

    let rDatacenterLarge2 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        let pref2 = reb.End ["A"]
        [reb.Build pref1; reb.Build pref2]

    let rDatacenterLarge3 (reb: Regex.REBuilder) =
        let pref1 = reb.Inter [reb.Through ["M"]; reb.End ["A"]]
        let pref2 = reb.Inter [reb.Through ["N"]; reb.End ["A"]]
        let pref3 = reb.End ["A"]
        [reb.Build pref1; reb.Build pref2; reb.Build pref3]

    let rBrokenTriangle1 (reb: Regex.REBuilder) =
        let pref1 = reb.Union [reb.Path ["C"; "A"; "E"; "D"]; reb.Path ["A"; "B"; "D"]]
        [reb.Build pref1]

    let rBigDipper1 (reb: Regex.REBuilder) =
        let op1 = reb.Path ["C"; "A"; "E"; "D"]
        let op2 = reb.Path ["A"; "E"; "D"]
        let op3 = reb.Path ["A"; "D"]
        let pref1 = reb.Union [op1; op2; op3]
        [reb.Build pref1]

    let rBadGadget1 (reb: Regex.REBuilder) =
        let op1 = reb.Path ["A"; "C"; "D"]
        let op2 = reb.Path ["B"; "A"; "D"]
        let op3 = reb.Path ["C"; "B"; "D"]
        let pref1 = reb.Union [op1; op2; op3]
        let op4 = reb.Path ["A"; "D"]
        let op5 = reb.Path ["B"; "D"]
        let op6 = reb.Path ["C"; "D"]
        let pref2 = reb.Union [op4; op5; op6]
        [reb.Build pref1; reb.Build pref2]

    let rBadGadget2 (reb: Regex.REBuilder) =
        let op1 = reb.Path ["A"; "C"; "D"]
        let op2 = reb.Path ["B"; "A"; "D"]
        let op3 = reb.Path ["C"; "B"; "D"]
        let op4 = reb.Path ["A"; "D"]
        let op5 = reb.Path ["B"; "D"]
        let op6 = reb.Path ["C"; "D"]
        let pref1 = reb.Union [op1; op2; op3; op4; op5; op6]
        [reb.Build pref1]

    let rSeesaw1 (reb: Regex.REBuilder) = 
        let op1 = reb.Path ["A"; "X"; "N"; "M"]
        let op2 = reb.Path ["B"; "X"; "N"; "M"]
        let op3 = reb.Path ["A"; "X"; "O"; "M"]
        let op4 = reb.Path ["X"; "O"; "M"]
        let pref1 = reb.Union [op1; op2; op3; op4]
        let pref2 = reb.Path ["X"; "N"; "M"]
        [reb.Build pref1; reb.Build pref2]

    let rStretchingManWAN1 (reb: Regex.REBuilder) = 
        let pref1 = reb.Concat [reb.Star reb.Outside; reb.Loc "A"; reb.Star reb.Inside; reb.Loc "Y"]
        let pref2 = reb.Concat [reb.Star reb.Outside; reb.Loc "B"; reb.Star reb.Inside; reb.Outside]
        [reb.Build pref1; reb.Build pref2]

    let rStretchingManWAN2 (reb: Regex.REBuilder) = 
        let pref1 = reb.Concat [reb.Outside; reb.Loc "A"; reb.Star reb.Inside; reb.Loc "Y"]
        let pref2 = reb.Concat [reb.Outside; reb.Loc "B"; reb.Star reb.Inside; reb.Outside]
        [reb.Build pref1; reb.Build pref2]

    let rStretchingManWAN3 (reb: Regex.REBuilder) = 
        let pref1 = 
            reb.Concat [
                reb.Star reb.Outside; 
                reb.Loc "A"; reb.Star reb.Inside; 
                reb.Loc "Y"; reb.Star reb.Outside; 
                reb.Loc "ASChina" ]
        [reb.Build pref1]

    let rStretchingManWAN4 (reb: Regex.REBuilder) = 
        let pref1 = reb.Concat [reb.Loc "W"; reb.Loc "A"; reb.Loc "C"; reb.Loc "D"; reb.Outside]
        let pref2 = reb.Concat [reb.Loc "W"; reb.Loc "B"; reb.Internal(); reb.Outside]
        [reb.Build pref1; reb.Build pref2]

    let rPinCushionWAN1 (reb: Regex.REBuilder) =
        let pref1 = reb.Concat [reb.Loc "W"; reb.Internal(); reb.Loc "Y"]
        let pref2 = reb.Concat [reb.Loc "X"; reb.Internal(); reb.Loc "Z"]
        [reb.Build pref1; reb.Build pref2]

    let rBackboneWAN1 (reb: Regex.REBuilder) =
        let pref1 = reb.End(["A"])
        [reb.Build pref1]

    let tests (settings: Args.T) = 
        let controlIn = settings.UseMed || settings.UsePrepending
        let noExport = settings.UseNoExport

        [
        
        {Name= "Diamond1";
         Explanation="A simple path";
         Topo= tDiamond;
         Rf= rDiamond1; 
         Receive= Some [("Y", "B"); ("N","Y"); ("X","N"); ("A","X")];
         Originate = Some ["B"];
         Prefs = Some [];
         Fail = None};

        {Name= "Diamond2";
         Explanation="Impossible Backup (should fail)";
         Topo= tDiamond;
         Rf= rDiamond2; 
         Receive= None;
         Originate = None;
         Prefs = None; 
         Fail = Some FRInconsistentPrefs};

        {Name= "DCsmall1";
         Explanation="Shortest paths routing";
         Topo= tDatacenterSmall;
         Rf= rDatacenterSmall1; 
         Receive= Some [];
         Originate = Some ["A"; "B"; "C"; "D"];
         Prefs = Some [];
         Fail = None};
       
        {Name= "DCsmall2";
         Explanation="Through spine no backup (should fail)";
         Topo= tDatacenterSmall;
         Rf= rDatacenterSmall2; 
         Receive= None;
         Originate = None;
         Prefs = None; 
         Fail = Some FRNoPathForRouters};

        {Name= "DCsmall3";
         Explanation="Through spine with backup";
         Topo= tDatacenterSmall;
         Rf= rDatacenterSmall3; 
         Receive= Some [("X", "A"); ("M", "X"); ("N", "X"); 
                        ("B", "X"); ("Y", "M"); ("Y", "N"); 
                        ("C", "Y"); ("D", "Y")];
         Originate = Some ["A"];
         Prefs = Some [("Y", "M", "N")]; 
         Fail = None};

        {Name= "DCsmall4";
         Explanation="End at single location";
         Topo= tDatacenterSmall;
         Rf= rDatacenterSmall4; 
         Receive= Some [("X", "A"); ("M","X"); ("N", "X"); 
                        ("Y", "M"); ("Y", "N"); ("C", "Y"); ("D", "Y")];
         Originate = Some ["A"];
         Prefs = Some []; 
         Fail = None };

        {Name= "DCsmall5";
         Explanation="Through spine to single location (should fail)";
         Topo= tDatacenterSmall;
         Rf= rDatacenterSmall5; 
         Receive= Some [("X", "A"); ("M", "X"); ("N", "X"); 
                        ("Y", "M"); ("Y", "N"); ("C", "Y"); 
                        ("D", "Y"); ("C", "Y")];
         Originate = Some ["A"];
         Prefs = Some [("Y", "M", "N")];
         Fail = None};

        {Name= "DCmedium1";
         Explanation="Shortest paths routing";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium1; 
         Receive= Some [];
         Originate = Some [];
         Prefs = Some [];
         Fail = None};

        {Name= "DCmedium2";
         Explanation="Through spine (should fail)";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium2; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs}; 

        {Name= "DCmedium3";
         Explanation="Through spine, valley free (should fail)";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium3; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "DCmedium4";
         Explanation="Through spine, valley free with simple backup";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium4; 
         Receive= Some [("G", "F"); ("H", "F"); ("E","G"); ("E","H"); ("X", "G"); 
                        ("X","H"); ("Y","G"); ("Y", "H"); ("C","X"); ("C","Y"); 
                        ("D","X"); ("D","Y"); ("A","C"); ("A","D"); ("B","C"); ("B", "D"); 
                        ("H","X"); ("H","Y"); ("G","X"); ("G","Y")]; (* Strange, but safe *)
         Originate = Some ["F"];
         Prefs = Some [("C", "X", "Y"); ("D", "X", "Y"); ("G", "F", "X"); 
                       ("G", "F", "Y"); ("H", "F", "X"); ("H", "F", "Y")];
         Fail = None};

        {Name= "DCmedium5";
         Explanation="Spine Preferences, no backup (should fail)";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium5; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "DCmedium6";
         Explanation="Spine Preferences with backup (should fail)";
         Topo= tDatacenterMedium;
         Rf= rDatacenterMedium6; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "DClarge1";
         Explanation="Through spine (should fail)";
         Topo= tDatacenterLarge;
         Rf= rDatacenterLarge1; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRNoPathForRouters};

        {Name= "DClarge2";
         Explanation="Through spine with backup (should fail)";
         Topo= tDatacenterLarge;
         Rf= rDatacenterLarge2; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "DClarge3";
         Explanation="Through spines w/prefs + backup (should fail)";
         Topo= tDatacenterLarge;
         Rf= rDatacenterLarge3; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "BrokenTriangle";
         Explanation="Inconsistent path suffixes (should fail)";
         Topo= tBrokenTriangle;
         Rf= rBrokenTriangle1; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRNoPathForRouters};

        {Name= "BigDipper";
         Explanation="Must choose the correct preference";
         Topo= tBigDipper;
         Rf= rBigDipper1; 
         Receive= Some [("E", "D"); ("A", "E"); ("C", "A")];
         Originate = Some ["D"];
         Prefs = Some [("A","E","D")];
         Fail = None};

        {Name= "BadGadget";
         Explanation="Total ordering prevents instability (should fail)";
         Topo= tBadGadget;
         Rf= rBadGadget1; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        {Name= "OkGadget";
         Explanation="Must find correct total ordering";
         Topo= tBadGadget;
         Rf= rBadGadget2; 
         Receive= Some [("A", "D"); ("B", "D"); ("C", "D")];
         Originate = Some ["D"];
         Prefs = Some [("A", "D", "C"); ("B", "D", "A"); ("C", "D", "B")];
         Fail = None};

        {Name= "Seesaw";
         Explanation="Must get all best preferences (should fail)";
         Topo= tSeesaw;
         Rf= rSeesaw1; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRInconsistentPrefs};

        (* TODO: test preferences on filters *)
        (if controlIn then
            {Name= "StretchingMan1";
             Explanation="Can't control inbound traffic";
             Topo= tStretchingManWAN;
             Rf= rStretchingManWAN1; 
             Receive= Some [("D", "Y"); ("D", "Z"); ("C", "D"); 
                            ("A", "C"); ("B", "C")];
             Originate = Some [];
             Prefs = Some [];
             Fail = None}
        else
            {Name= "StretchingMan1";
             Explanation="Can't control inbound traffic";
             Topo= tStretchingManWAN;
             Rf= rStretchingManWAN1; 
             Receive= None;
             Originate = None;
             Prefs = None;
             Fail = Some FRCantControlPeers});

        (if controlIn && noExport then
            {Name= "StretchingMan2";
             Explanation="Prefer one AS over another";
             Topo= tStretchingManWAN;
             Rf= rStretchingManWAN2; 
             Receive= Some [("C", "D"); ("A","C"); ("B", "C")];
             Originate = Some [];
             Prefs = Some [("D", "Y", "Z")];
             Fail = None};
         else
            {Name= "StretchingMan2";
             Explanation="Prefer one AS over another";
             Topo= tStretchingManWAN;
             Rf= rStretchingManWAN2; 
             Receive= None;
             Originate = None;
             Prefs = None;
             Fail = Some FRCantControlPeers});

        {Name= "StretchingMan3";
         Explanation="Using peer not listed in the topology";
         Topo= tStretchingManWAN;
         Rf= rStretchingManWAN3; 
         Receive= Some [];
         Originate = Some [];
         Prefs = Some [];
         Fail = None};

         // TODO: test filters etc
         (if controlIn && noExport then
            {Name= "StretchingMan4";
             Explanation="Use MED, Prepending, No-export, and Local-pref";
             Topo= tStretchingManWAN2;
             Rf= rStretchingManWAN4; 
             Receive= Some [];
             Originate = Some [];
             Prefs = Some [];
             Fail = None};
         else 
            {Name= "StretchingMan4";
             Explanation="Use MED, Prepending, No-export, and Local-pref";
             Topo= tStretchingManWAN2;
             Rf= rStretchingManWAN4; 
             Receive= None;
             Originate = None;
             Prefs = None;
             Fail = Some FRCantControlPeers});
         
        {Name= "PinCushion";
         Explanation="Unimplementable peer preference";
         Topo= tPinCushionWAN;
         Rf= rPinCushionWAN1; 
         Receive= None;
         Originate = None;
         Prefs = None;
         Fail = Some FRCantControlPeers};

        {Name= "Backbone";
         Explanation="Incoming traffic from multiple peers";
         Topo= tBackboneWAN;
         Rf= rBackboneWAN1; 
         Receive= Some [("NY", "A"); ("SEA", "A")];
         Originate = Some ["A"];
         Prefs = Some [];
         Fail = None};
    ]

    let testAggregationFailure () = 
        printf "Aggregation failures "
        let topo = Topology.Examples.topoDatacenterMedium () 
        let reb = Regex.REBuilder(topo)
        let pol = reb.End ["A"]
        let aggs = Map.add "X" [(Prefix.prefix (10u, 0u, 0u, 0u) 31u, Seq.ofList ["PEER"])] Map.empty
        let aggs = Map.add "Y" [(Prefix.prefix (10u, 0u, 0u, 0u) 31u, Seq.ofList["PEER"])] aggs
        let res = compileToIR "" 0 (Predicate.prefix (10u, 0u, 0u, 0u) 32u) aggs reb [reb.Build pol]
        match res with
        | Err _ -> failed ()
        | Ok(res) -> 
            match res.K with 
            | Some (1,_,_,_,_) -> passed ()
            | _ -> failed ()

    let testCompilation () =
        let border = String.replicate 80 "-"
        printfn "%s" border
        let settings = Args.getSettings ()
        let tests = tests settings
        let longestName = List.maxBy (fun t -> t.Name.Length) tests
        let longestName = longestName.Name.Length
        let longestDesc = List.maxBy (fun t -> t.Explanation.Length) tests
        let longestDesc = longestDesc.Explanation.Length
        for test in tests do
            let spacesName = String.replicate (longestName - test.Name.Length + 3) " "
            let spacesDesc = String.replicate (longestDesc - test.Explanation.Length + 3) " "
            let msg = sprintf "%s%s%s%s" test.Name spacesName test.Explanation spacesDesc
            printf "%s" msg
            logInfo1(0, "\n" + msg)
            let (ain, _) = Topology.alphabet test.Topo
            let ain = ain |> Set.map (fun v -> v.Loc)
            let reb = Regex.REBuilder(test.Topo)
            let built = test.Rf reb
            if not (Topology.isWellFormed test.Topo) then 
                failed ()
                logInfo1(0, msg)
            else
                let pred = Predicate.top
                match compileToIR (settings.DebugDir + test.Name) 0 pred Map.empty reb built with 
                | Err(x) ->
                    if (Option.isSome test.Receive || 
                        Option.isSome test.Originate || 
                        Option.isSome test.Prefs || 
                        Option.isNone test.Fail) then 
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, should compile but did not" test.Name
                        logInfo1(0, msg)
                    match test.Fail, x with 
                    | Some FRNoPathForRouters, NoPathForRouters _ -> passed ()
                    | Some FRInconsistentPrefs, InconsistentPrefs _ -> passed ()
                    | Some FRCantControlPeers, UncontrollableEnter _ -> passed ()
                    | Some FRCantControlPeers, UncontrollablePeerPreference _ -> passed ()
                    | _ ->
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, expected error" test.Name
                        logInfo1(0, msg)
                | Ok(res) -> 
                    let config = res.Config
                    if (Option.isNone test.Receive || 
                        Option.isNone test.Originate || 
                        Option.isNone test.Prefs || 
                        Option.isSome test.Fail) then
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, should not compile but did" test.Name
                        logInfo1(0, msg)
                    else
                        let mutable fail = false
                        let rs = Option.get test.Receive
                        for (x,y) in rs do 
                            if not (receiveFrom ain config x y) then
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should receive from %s but did not" test.Name x y
                                logInfo1(0, msg)
                        let os = Option.get test.Originate
                        for x in os do 
                            if not (originates config x) then 
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should originate a route but did not" test.Name x
                                logInfo1(0, msg)
                        let ps = Option.get test.Prefs
                        for (x,a,b) in ps do
                            if not (prefersPeer ain config x (a,b)) then 
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should prefer %s to %s but did not" test.Name x a b
                                logInfo1(0, msg)
                        if fail 
                        then failed ()
                        else passed ()
        printfn "%s" border


    let run () =
        testAggregationFailure ()
        testCompilation ()
