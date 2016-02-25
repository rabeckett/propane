module Abgp

open CGraph 
open Common
open Common.Debug
open Common.Error
open Common.Format
open System.Collections.Generic

exception UncontrollableEnterException of string
exception UncontrollablePeerPreferenceException of string

/// Collection of IR types organized as follows:
///
/// Config type T maps each router name to a RouterConfig
/// RouterConfig maps predicates to a collection of filters
/// Filters can either allow or deny. If they allow, then they specify:
///    - What they match - peer, community, regex filter
///    - The preference of the match (local-pref)
///    - A collection of exports for the match (updated local-pref, community, peer)

type LocalPref = int
type Community = string

type Peer = 
    | Any
    | In
    | Out
    | Router of string

    override x.ToString() =
        match x with
        | Any -> "*"
        | In -> "in"
        | Out -> "out"
        | Router y -> y


type Match = 
    | Peer of Peer 
    | State of Community * Peer
    | PathRE of Regex.T

    override x.ToString () = 
        match x with 
        | Peer p -> "Peer=" + (string p)
        | State(is,p) -> "Community=" + is + ", Peer=" + (string p)
        | PathRE r -> "Regex=" + string r


type Modification = 
    | SetComm of string
    | SetMed of int
    | PrependPath of int

    override this.ToString() = 
        match this with 
        | SetComm(is) -> "Community<-" + is
        | SetMed i -> "MED<-" + string i
        | PrependPath i -> "Prepend " + string i

type Import = Match * LocalPref
type Export = Peer * Modification list

type Filter = 
    | Deny
    | Allow of Import * (Export list)

type Actions =
    | Originate
    | Filters of Filter list

type DeviceAggregates = (Prefix.T * seq<string>) list
type DeviceTags = ((string * Prefix.T list) * seq<string>) list
type DeviceMaxRoutes = (uint32 * seq<string>) list

type DeviceControl = 
    {Aggregates: DeviceAggregates;
     Tags: DeviceTags;
     MaxRoutes: DeviceMaxRoutes}

type RouterConfig = 
    {Actions: (Predicate.T * Actions) list;
     Control: DeviceControl}

type T =
    {PolInfo: Ast.PolInfo; 
     RConfigs: Map<string, RouterConfig>}

type CounterExample = 
    | NoPathForRouters of Set<string>
    | InconsistentPrefs of CgState * CgState
    | UncontrollableEnter of string
    | UncontrollablePeerPreference of string

/// Functions that operate over final per-router 
/// configurations, and display them in a nice, 
/// human-readable format w/indentation.

open Core.Printf

let formatActions (sb: System.Text.StringBuilder) pred (actions: Actions) =
    let predStr = string pred
    match actions with 
    | Originate -> bprintf sb "\n  Originate: [Pred=%s]" predStr 
    | Filters fs ->
        for f in fs do
            match f with 
            | Deny ->
                bprintf sb "\n  Deny: "
                bprintf sb "[Pred=%s]" predStr
            | Allow ((m, lp), es) ->
                bprintf sb "\n  Allow: [Pred=%s, %s]" predStr (string m)
                if lp <> 100 then
                    bprintf sb " (LP=%d)" lp
                for (peer, acts) in es do
                    bprintf sb "\n    Export: [Peer<-%s" (string peer)
                    if acts <> [] then
                        let str = Common.List.joinBy "," (List.map string acts)
                        sb.Append(", " + str) |> ignore
                    bprintf sb "]"

let formatPred (pred: Predicate.T) (racts: Map<string, Actions>) =
    let sb = System.Text.StringBuilder ()
    Map.iter (fun (router:string) act -> 
        bprintf sb "\nRouter %s" router
        formatActions sb pred act) racts
    sb.ToString()

let format (config: T) =
    let sb = System.Text.StringBuilder ()
    for kv in config.RConfigs do
        let routerName =
            Topology.router kv.Key config.PolInfo.Ast.TopoInfo
        let routerConfig = kv.Value
        bprintf sb "\nRouter %s" routerName
        for (prefix, peers) in routerConfig.Control.Aggregates do
            for peer in peers do
                bprintf sb "\n  Aggregate(%s, %s)" (string prefix) peer
        for ((c, prefix), peers) in routerConfig.Control.Tags do 
            for peer in peers do 
                bprintf sb "\n  Tag(%s, %s, %s)" c (string prefix) peer
        for (i, peers)  in routerConfig.Control.MaxRoutes do 
            for peer in peers do 
                bprintf sb "\n  MaxRoutes(%d)" i
        for (pred, actions) in routerConfig.Actions do
            formatActions sb pred actions
        bprintf sb "\n" 
    sb.ToString()

/// Minimization helper functions for reducing the size of 
/// configurations. Minimization occurs at 3 levels of granularity:
/// 1. Per product graph node - used during generation for memory efficiency
/// 2. Per prefix - allows us to minimize based on an equivalence class
/// 3. Per router - minimize for a router (e.g., fall-through elimination)

let size (config: T) =
    let mutable count = 0 
    for kv in config.RConfigs do 
        let rconf = kv.Value
        for (_,actions) in rconf.Actions do
            match actions with 
            | Originate -> count <- count + 1
            | Filters fs ->
                for f in fs do 
                    match f with
                    | Deny -> count <- count + 1
                    | Allow ((m,_),es) -> 
                        count <- count + (List.length es)
    count

let private chooseAction f (filt: Filter) =
    let inline adjActs (peer,acts) =
        peer, List.choose f acts
    match filt with 
    | Deny -> Deny
    | Allow ((m,lp),es) ->
        Allow ((m,lp), List.map adjActs es)

module NodeWide =

    let removeRedundantTag m exports = 
        match m with 
        | Match.Peer _ | Match.PathRE _ -> exports 
        | Match.State(is,_) -> 
            List.choose (fun (peer, acts) -> 
                let acts' = List.filter (fun a -> 
                    match a with 
                    | SetComm c -> c <> is
                    | _ -> true) acts
                Some (peer, acts') ) exports

    let removeCommMatchForUnqEdges cg (eCounts : Dictionary<_,_> ) v m = 
        let inline unq e =
            let mutable res = 0
            eCounts.TryGetValue(e,&res) && res = 1
        match m with 
        | Match.State (c,peers) -> 
            match peers with 
            | Any | In ->
                let ins = CGraph.neighborsIn cg v |> Seq.map CGraph.loc
                if Seq.forall (fun i -> unq (i, v.Node.Loc)) ins then Match.Peer(peers) else m 
            | Router peer -> 
                if unq (peer, v.Node.Loc) then Match.Peer(Router peer)
                else m
            | Out -> Match.Peer(Out)
        | _ -> m

    let minimize cg eCounts v m exports =
        let exports' = removeRedundantTag m exports 
        let m' = removeCommMatchForUnqEdges cg eCounts v m
        (exports', m')

module PrefixWide = 

    let inline private allCommMatches filters = 
        Common.List.fold (fun acc f -> 
            match f with 
            | Allow ((Match.State(c,_),lp), es) ->
                Set.add c acc
            | _ -> acc) Set.empty filters

    let private removeUnobservedTag filters = 
        let matchedComms = allCommMatches filters
        List.map (fun f -> 
            chooseAction (fun a ->
                match a with
                | SetComm(c) when not (Set.contains c matchedComms) -> None
                | _ -> Some a) f
        ) filters

    let private combineInOut filters = 
        match filters with
        | (Allow ((Peer s1,lp1),es1) as hd1) :: 
          (Allow ((Peer s2,lp2),es2) as hd2) :: tl -> 
            match s1, s2 with 
            | In, Out | Out, In -> 
                if (lp1 = lp2) && (es1 = es2) then 
                    Allow ((Match.Peer Any, lp1),es1) :: tl
                else filters
            | _, _ -> filters
        | _ -> filters

    let minimize filters =
        filters
        |> removeUnobservedTag
        |> combineInOut 

module RouterWide = 

    let inline disjointMatch m1 m2 =
        match m1, m2 with
        | Peer(x), Peer(y)
        | State(_,x), Peer(y) ->
            match x,y with 
            | Any, _ -> false
            | _, Any -> false
            | In, Out 
            | Out, In -> true
            | _, _ -> false
        | _, _ -> true

    let inline disjointFilter (f1: Filter) (f2: Filter) =
        match f1, f2 with
        | Deny, _ -> false
        | _, Deny -> false
        | Allow ((m1,_),es1), Allow ((m2,_),es2) -> 
            disjointMatch m1 m2

    let inline eqExports (f1: Filter) (f2: Filter) = 
        match f1, f2 with
        | Deny, Deny -> true
        | Deny, _ -> false
        | _, Deny -> false
        | Allow (_,es1), Allow (_,es2) -> es1 = es2

    let inline coveredMatch m1 m2 =
        match m1, m2 with
        | Peer(x), Peer(y)
        | State(_,x), Peer(y) ->
            y = Any || x = y
        | _, _ -> true

    let inline coveredFilter (f1: Filter) (f2: Filter) = 
        match f1, f2 with
        | Deny, Allow _ -> false
        | Allow _, Deny -> false
        | Deny, Deny -> true
        | Allow ((m1,_),es1), Allow ((m2,_),es2) -> 
            (coveredMatch m1 m2) && (es1 = es2)

    let makePairs (pairs: (Predicate.T * Actions) list) = 
        let mutable res = [] 
        for (pred, actions) in pairs do
            match actions with
            | Originate -> () 
            | Filters fs ->
                for f in fs do 
                    res <- (pred,f) :: res
        List.rev res

    let inline unMakePairs pairs = 
        Seq.groupBy fst pairs
        |> Seq.map (fun (x,ys) -> (x, Seq.map snd ys |> List.ofSeq)) 
        |> List.ofSeq

    let inline overlap (p1,f1) (p2,f2) =
        not (disjointFilter f1 f2) && not (Predicate.disjoint p1 p2)

    let addBackPair origins pairs acc (p,_) =
        if Set.contains p origins then (p,Originate) :: acc else 
        match Map.tryFind p pairs with 
        | None -> acc
        | Some fs -> (p, Filters fs) :: acc

    let inline addOriginator acc (p,actions) =
        match actions with 
        | Originate -> Set.add p acc
        | _ -> acc

    let rec fteAux pairs =
        match pairs with 
        | [] -> []
        | ((p1,f1) as pair)::tl ->
            let tl = fteAux tl
            match List.tryFind (overlap pair) tl with
            | None -> pair :: tl
            | Some (p2,f2) ->
                if (coveredFilter f1 f2) && (Predicate.implies p1 p2) 
                then tl
                else pair :: tl

    let fallThroughElimination (rconfig: RouterConfig) : RouterConfig =
        let origins = Common.List.fold addOriginator Set.empty rconfig.Actions
        let pairs = fteAux (makePairs rconfig.Actions) |> unMakePairs |> Map.ofList
        let actions = Common.List.fold (addBackPair origins pairs) [] rconfig.Actions
        {Control = rconfig.Control; Actions = List.rev actions}

    let minimize (config: T) = 
        let settings = Args.getSettings () 
        let map = 
            if settings.Parallel 
            then Array.Parallel.map 
            else Array.map
        let rconfs = config.RConfigs
        let rconfs = 
            rconfs
            |> Map.toArray 
            |> map (fun (r,rconf) -> r, fallThroughElimination rconf)
            |> Map.ofArray
        {PolInfo = config.PolInfo; RConfigs = rconfs}

/// Compilation from CGraph (product graph) to a complete IR config.
/// Compiles for each prefix in parallel and then merges the results
/// together, sequentially, to get a per-router configuration.
/// Minimization per-router the occurs in parallel.

type AggregationSafety = 
    {NumFailures: int; 
     PrefixLoc: string; 
     AggregateLoc: string; 
     Prefix: Prefix.T;
     Aggregate: Prefix.T}

type PredConfig = Predicate.T * Map<string, Actions>

type PrefixResult =
    {K: AggregationSafety option;
     BuildTime: int64;
     MinimizeTime: int64;
     OrderingTime: int64;
     ConfigTime: int64;
     Config: PredConfig}

type PrefixCompileResult = Result<PrefixResult, CounterExample>

type Stats = 
    {NumPrefixes: int;
     ConfigSize: int;
     PrefixTime: int64;
     PerPrefixTimes: int64 array;
     PerPrefixBuildTimes: int64 array;
     PerPrefixMinTimes: int64 array;
     PerPrefixOrderTimes: int64 array;
     PerPrefixGenTimes: int64 array;
     JoinTime: int64;
     MinTime: int64}

type CompilationResult =
    {Abgp: T;
     AggSafety: AggregationSafety option;
     Stats: Stats}

let joinConfigs polInfo (aggs, comms, maxroutes) (results: PrefixResult list) : T =
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
    let routerConfigs = 
        Map.map (fun router vs ->
            let a = Common.Map.getOrDefault router [] aggs
            let b = Common.Map.getOrDefault router [] comms
            let c = Common.Map.getOrDefault router [] maxroutes
            {Actions=List.rev vs; Control={Aggregates=a; Tags=b; MaxRoutes=c}}) result
    {PolInfo = polInfo; RConfigs = routerConfigs}

/// Ensure well-formedness for controlling 
/// traffic entering the network. MED and prepending allow
/// certain patterns of control to immediate neighbors only

module Incoming =

    type IncomingPattern = 
        | Anything
        | Nothing of string
        | Specific of Regex.T

    type IncomingInfo = 
        {Peers: seq<CgState>;
         Info: Map<CgState, IncomingPattern>}

    type IncomingExportMap = Map<CgState, Modification list>

    let collectForPeer cg acc peer = 
        let reachable = Reachable.dfs cg peer Down
        let reach = ResizeArray()
        let mutable hasRepeatedOut = false 
        for v in reachable do 
            if v <> peer && Topology.isTopoNode v.Node then
                reach.Add(v)
                if CGraph.isRepeatedOut cg v then 
                    hasRepeatedOut <- true
        let hasOther = (reach.Count > 1) || (not hasRepeatedOut && reach.Count > 0)
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
                else 
                    let msg = sprintf "enable no-export to limit incoming traffic to peer %s" x
                    raise (UncontrollableEnterException msg)
            | Specific re -> 
                let msg = sprintf "(%s) incoming traffic cannot conform to: %s" p.Node.Loc (string re)
                raise (UncontrollableEnterException msg)
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
            |> Seq.map (fun p -> (Bitset32.minimum (Reachable.srcAccepting cg p Down), p))
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

/// Ensure well-formedness for outgoing 
/// traffic leaving the network. Use regex filters 
/// to control advertisements and rewrite as peers when possible.

module Outgoing = 

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

let getMatches (allPeers, inPeers, outPeers) outgoingMatches =
    // get peer topology location information
    let peermatches, regexes = List.partition Outgoing.isPeerMatch (List.ofSeq outgoingMatches)
    let peermatches = List.map Outgoing.getPeerMatch peermatches
    let mutable peersIn = Set.empty 
    let mutable peerLocsIn = Set.empty
    let mutable peersOut = Set.empty
    let mutable peerLocsOut = Set.empty
    for v in peermatches do
        if Topology.isInside v.Node then
            peersIn <- Set.add v peersIn
            peerLocsIn <- Set.add v.Node.Loc peerLocsIn
        else
            peersOut <- Set.add v peersOut
            peerLocsOut <- Set.add v.Node.Loc peerLocsOut
    let mutable peers = Set.union peersIn peersOut
    let mutable peerLocs = Set.union peerLocsIn peerLocsOut
    // get peer state product graph information
    let mutable matches = List.map (fun r -> Match.PathRE (Outgoing.getRegexMatch r)) regexes
    let mutable statesIn = Set.empty
    let mutable statesOut = Set.empty
    for v in peers do
        if Topology.isInside v.Node
        then statesIn <- Set.add v.State statesIn
        else statesOut <- Set.add v.State statesOut
    let states = Set.union statesIn statesOut
    // check if possible to compress matches
    if peerLocs = allPeers && (states.Count = 1) then
        matches <- Match.State(string (Set.minElement states), Any) :: matches
        peers <- Set.empty
    else 
        if peerLocsIn = inPeers && (statesIn.Count = 1) && not peerLocsIn.IsEmpty then 
            matches <- Match.State(string (Set.minElement statesIn), In) :: matches
            peers <- Set.difference peers peersIn
        if peerLocsOut = outPeers && (statesOut.Count = 1) && not peerLocsOut.IsEmpty then 
            matches <- Match.Peer(Out) :: matches
            peers <- Set.difference peers peersOut
        for v in peers do
            let m = 
                if Topology.isInside v.Node then  
                    Match.State(string v.State, Router v.Node.Loc)
                else Match.Peer(Router v.Node.Loc)
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
    let insideExport = [(In, [SetComm (string x.State)])]
    let specialCase = ref false
    let exports = List.ofSeq toOutside |> List.map (fun v -> (Router v.Node.Loc, getExport specialCase inExports x v)) 
    if not !specialCase then
        let sendToLocs = Seq.map (fun v -> v.Node.Loc) outgoing |> Set.ofSeq
        let sendToLocs = 
            match unqMatchPeer with
            | Some x -> Set.add x sendToLocs
            | None -> sendToLocs
        if sendToLocs.IsSupersetOf outPeers then
            [(Any, [SetComm(string x.State)])]
        else exports @ insideExport
    else  exports @ insideExport

let inline edgeCounts (cg: CGraph.T) =
    let counts = Dictionary()
    for e in cg.Graph.Edges do 
        let key = (e.Source.Node.Loc, e.Target.Node.Loc)
        let mutable value = 0
        if counts.TryGetValue(key, &value) then 
            counts.[key] <- value + 1
        else
            counts.Add(key,1)
    counts

let getPeerInfo (vs: seq<Topology.Node>) =
    let mutable allIn = Set.empty
    let mutable allOut = Set.empty
    for v in vs do
        if Topology.isInside v then
            let r = v.Loc
            allIn <- Set.add r allIn
        else if Topology.isOutside v then 
            let r = v.Loc 
            allOut <- Set.add r allOut 
    let all = Set.union allIn allOut
    (all, allIn, allOut)

let genConfig (cg: CGraph.T) 
              (pred: Predicate.T) 
              (ord: Consistency.Ordering) 
              (inExports: Incoming.IncomingExportMap) 
              : PredConfig =
    let settings = Args.getSettings ()
    let ain, aout = Topology.alphabet cg.Topo 
    let ain = ain |> Set.map (fun v -> v.Loc)
    let aout = aout |> Set.map (fun v -> v.Loc)
    let eCounts = edgeCounts cg
    let mutable config = Map.empty
    // generate a config for each internal router
    for router in ain do
        let mutable filters = []
        let mutable originates = false
        // look at the nodes according to preference
        let mutable prefs = Seq.empty 
        if ord.TryGetValue(router, &prefs) then
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
                let peerTypes = Seq.map (Outgoing.getOutPeerType cg) receiveFrom
                let origin = Seq.exists ((=) cg.Start) nsIn
                // get the compressed set of matches using *, in, out when possible
                let matches =  if origin then [] else getMatches inPeerInfo peerTypes
                // get the compressed set of exports taking into account if there is a unique receive peer
                let unqMatchPeer = 
                    match matches with 
                    | [Match.Peer (Router x)] -> Some x
                    | [Match.State(_,(Router x))] -> Some x 
                    | _ -> None
                let exports = getExports outPeerInfo cgstate inExports sendTo unqMatchPeer 
                // match/export local minimizations
                for m in matches do 
                    let exports, m = 
                        if settings.Minimize
                        then NodeWide.minimize cg eCounts cgstate m exports
                        else exports, m
                    filters <- Allow ((m,lp), exports) :: filters
                originates <- origin || originates
        // prefix-wide minimizations
        filters <- 
            if settings.Minimize
            then PrefixWide.minimize filters
            else filters
        // no need for explicit deny if we allow everything
        match filters with 
        | [Allow ((Match.Peer Any,_), _)] -> ()
        | _ -> filters <- List.rev (Deny :: filters)
        // build the final configuration
        let deviceConf = 
            if originates 
            then Originate
            else Filters filters
        config <- Map.add router deviceConf config
    (pred, config)

let inline insideLoc v = 
    if Topology.isInside v.Node then 
        Some (CGraph.loc v) 
    else None

let inline insideOriginatorLoc v =
    if (Topology.isInside v) && 
       (Topology.canOriginateTraffic v) then Some v.Loc 
    else None

let inline insideOriginators cg =
    CGraph.neighbors cg cg.Start
    |> Seq.choose insideLoc
    |> Set.ofSeq

let getLocsThatCantGetPath idx cg (reb: Regex.REBuilder) dfas = 
    let startingLocs = Array.fold (fun acc dfa -> Set.union (reb.StartingLocs dfa) acc) Set.empty dfas
    let originators = insideOriginators cg
    let canOriginate = 
        cg.Topo.Vertices
        |> Seq.choose insideOriginatorLoc
        |> Set.ofSeq
    let locsThatNeedPath = Set.difference (Set.intersect startingLocs canOriginate) originators
    let locsThatGetPath = CGraph.acceptingLocations cg
    logInfo(idx, sprintf "Locations that need path: %s" (locsThatNeedPath.ToString()))
    logInfo(idx, sprintf "Locations that get path: %s" (locsThatGetPath.ToString()))
    Set.difference locsThatNeedPath locsThatGetPath

let getUnusedPrefs cg res = 
    let mutable nRegexes = Bitset32.empty 
    for i in 1..List.length res do 
        nRegexes <- Bitset32.add i nRegexes
    let prefs = CGraph.preferences cg
    Bitset32.difference nRegexes prefs // don't use difference here
    |> Bitset32.filter (fun i -> res.[i-1] <> Regex.empty)

let warnAnycasts cg (polInfo:Ast.PolInfo) pred =
    let settings = Args.getSettings()
    let origLocs = polInfo.OrigLocs.[pred]
    let orig = insideOriginators cg
    let bad = Set.difference orig origLocs
    let ti = polInfo.Ast.TopoInfo
    if (not settings.Anycast) && (Set.count orig > 1) then 
        let bad1 = bad.MinimumElement 
        let bad2 = (Set.remove bad1 bad).MinimumElement
        let bad1 = Topology.router bad1 ti
        let bad2 = Topology.router bad2 ti
        let msg =
            sprintf "Anycasting from multiple locations, e.g., %s and %s " bad1 bad2 +
            sprintf "for predicate %s. If you believe this is not a mistake, you can "  (string pred) +
            sprintf "enable anycast by using the -anycast:on flag"
        error msg
    if not (Set.isEmpty bad) then
        let bad1 = Topology.router (bad.MinimumElement) ti
        let bad2 = Topology.router (orig.MaximumElement) ti
        let msg =
            sprintf "Anycasting from multiple locations, e.g., %s and %s for  " bad1 bad2  +
            sprintf "predicate %s, even though the location is not explicitly " (string pred) +
            sprintf "mentioned in the policy. This is almost always a mistake."
        warning msg

let getMinAggregateFailures (cg: CGraph.T) pred (aggInfo: Map<string, DeviceAggregates>) = 
    let originators = CGraph.neighbors cg cg.Start
    let prefixes = Predicate.getPrefixes pred
    let smallest = ref System.Int32.MaxValue
    let pairs = ref None
    for p in prefixes do
        aggInfo |> Map.iter (fun aggRouter aggs ->
            let relevantAggs = List.filter (fun (prefix, _) -> Prefix.implies (Prefix.toPredicate [prefix]) p) aggs
            if not relevantAggs.IsEmpty then 
                let rAgg, _ = relevantAggs.Head
                match CGraph.Failure.disconnectLocs cg originators aggRouter with 
                | None -> () 
                | Some (k,x,y) ->
                    if k < !smallest then 
                        smallest := min !smallest k
                        let p = (x,y, List.head (Prefix.toPrefixes p), rAgg)
                        pairs := Some p)
    if !smallest = System.Int32.MaxValue then None else 
    let (x,y,p,agg) = Option.get !pairs
    Some {NumFailures = !smallest; 
          PrefixLoc = x; 
          AggregateLoc = y; 
          Prefix = p; 
          Aggregate = agg}

let compileToIR (fullName: string) 
                (idx: int) 
                (pred: Predicate.T) 
                (polInfo: Ast.PolInfo option) 
                (aggInfo: Map<string,DeviceAggregates>) 
                (reb: Regex.REBuilder) 
                (res: Regex.T list) 
                : PrefixCompileResult =
    let settings = Args.getSettings ()
    let fullName = fullName + "(" + (string idx) + ")"
    let dfas, dfaTime = Profile.time (List.map (fun r -> reb.MakeDFA (Regex.rev r))) res
    let dfas = Array.ofList dfas
    let cg, pgTime = Profile.time (CGraph.buildFromAutomata (reb.Topo())) dfas
    let buildTime = dfaTime + pgTime
    debug (fun () -> CGraph.generatePNG cg fullName )
    let cg, delTime = Profile.time (CGraph.Minimize.delMissingSuffixPaths) cg
    let cg, minTime = Profile.time (CGraph.Minimize.minimize idx) cg
    let minTime = delTime + minTime
    debug (fun () -> CGraph.generatePNG cg (fullName + "-min")) 
    // warn for anycasts 
    if not settings.Test then
        warnAnycasts cg polInfo.Value pred
    // check there is a route for each location specified
    let lost = getLocsThatCantGetPath idx cg reb dfas
    if not (Set.isEmpty lost) then Err(NoPathForRouters(lost)) else
    // Find unused preferences for policies that were not drop
    let unusedPrefs = getUnusedPrefs cg res
    if not (Bitset32.isEmpty unusedPrefs)  then
        Bitset32.iter (fun i -> 
            let msg = 
                sprintf "Unused preference %d for predicate " i +
                sprintf "%s for a non-drop policy" (string pred)
            warning msg) unusedPrefs
    try
        // check that BGP can ensure incoming traffic compliance
        let inExports = Incoming.configureIncomingTraffic cg
        // check aggregation failure consistency
        let k = getMinAggregateFailures cg pred aggInfo
        // check  that BGP preferences can be set properly
        let (ordering, orderTime) = Profile.time (Consistency.findOrderingConservative idx cg) fullName
        match ordering with 
        | Ok ord ->
            let config, configTime = Profile.time (genConfig cg pred ord) inExports
            let result = 
                {K=k; 
                 BuildTime=buildTime; 
                 MinimizeTime=minTime; 
                 OrderingTime=orderTime;
                 ConfigTime=configTime; 
                 Config=config}
            let msg = formatPred pred (snd config)
            debug (fun () -> System.IO.File.WriteAllText (sprintf "%s.ir" fullName, msg))
            Ok (result)
        | Err((x,y)) -> Err(InconsistentPrefs(x,y))
    with 
        | UncontrollableEnterException s -> Err(UncontrollableEnter s)
        | UncontrollablePeerPreferenceException s -> Err(UncontrollablePeerPreference s)


let compileForSinglePrefix (fullName:string) 
                           (idx:int) 
                           (polInfo: Ast.PolInfo) 
                           (aggInfo: Map<string, DeviceAggregates>)
                           ((pred, reb, res): Ast.PolicyPair) 
                           : PrefixResult =
    match compileToIR fullName idx pred (Some polInfo) aggInfo reb res with 
    | Ok(config) -> config
    | Err(x) ->
        let ti = polInfo.Ast.TopoInfo
        match x with
        | NoPathForRouters rs ->
            let routers = 
                Set.map (fun r -> Topology.router r ti) rs
                |> Common.Set.joinBy ", "
            let msg = 
                sprintf "Unable to find a path for routers: " + 
                sprintf "%s for predicate %s" routers (string pred)
            error msg
        | InconsistentPrefs(x,y) ->
            let l = Topology.router (CGraph.loc x) ti
            let msg = 
                sprintf "Cannot find preferences for router " + 
                sprintf "%s for predicate %s" l (string pred)
            error msg
        | UncontrollableEnter x -> 
            let l = Topology.router x ti
            let msg = 
                sprintf "Cannot control inbound traffic from " + 
                sprintf "peer: %s for predicate %s" l (string pred)
            error msg
        | UncontrollablePeerPreference x -> 
            let l = Topology.router x ti
            let msg =
                sprintf "Cannot control inbound preference from peer: %s for " l  +
                sprintf "predicate %s. Possibly enable prepending: -prepending:on" (string pred)
            error msg

let checkAggregateLocs ins _ prefix links = 
    if Set.contains "out" ins then
        let msg = 
            sprintf "Cannot aggregate on external location: " + 
            sprintf "out for prefix: %s" (string prefix)
        error msg
    match List.tryFind (fst >> Topology.isOutside) links with
    | None -> ()
    | Some x -> 
        let msg = 
            sprintf "Cannot aggregate on external location: " +
            sprintf "%s for prefix: %s" (fst x).Loc (string prefix)
        error msg

let checkCommunityTagLocs ins _ (c, prefix) links =
    if Set.contains "out" ins then
        let msg = 
            sprintf "Cannot tag communities on external location: out " + 
            sprintf "for community %s, prefix: %s" c (string prefix)
        error msg
    match List.tryFind (fst >> Topology.isOutside) links with
    | None -> ()
    | Some x -> 
        let msg = 
            sprintf "Cannot tag communities on external location: " + 
            sprintf "%s for community %s prefix: %s" (fst x).Loc c (string prefix)
        error msg

let checkMaxRouteLocs ins outs i links =
    let v = List.exists (fun (a,b) -> Topology.isOutside a && Topology.isOutside b) links
    let w = Set.contains "out" ins
    let x = Set.contains "out" outs
    let y = List.exists (fst >> Topology.isOutside) links
    let z = List.exists (snd >> Topology.isOutside) links
    if v || ((w || y) && (x || z)) then 
        let msg = 
            sprintf "Cannot set maxroutes(%d) on links " i +
            sprintf "without an edge in the internal topology"
        error msg

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

let splitConstraints (pi: Ast.PolInfo) =
    let aggs, comms, maxroutes = 
        Common.List.fold (fun ((x,y,z) as acc) c -> 
            match c with
            | Ast.CAggregate (p,ins,outs) -> ((p,ins,outs)::x, y, z)
            | Ast.CCommunity (s,p,ins,outs) -> (x, ((s,p),ins,outs)::y, z)
            | Ast.CMaxRoutes (i,ins,outs) -> (x, y, (i,ins,outs)::z)
            | _ -> acc
        ) ([], [], []) pi.CConstraints
    let topo = pi.Ast.TopoInfo.Graph
    let aggInfo = splitByLocation checkAggregateLocs topo aggs
    let commInfo = splitByLocation checkCommunityTagLocs topo comms
    let maxRouteInfo = splitByLocation checkMaxRouteLocs topo maxroutes
    (aggInfo, commInfo, maxRouteInfo)

let minFails x y = 
    match x, y with 
    | None, _ -> y 
    | _, None -> x 
    | Some a, Some b ->
        if a.NumFailures < b.NumFailures then x else y

let compileAllPrefixes (fullName: string) 
                       (polInfo: Ast.PolInfo) 
                       : CompilationResult =
    let settings = Args.getSettings () 
    let mapi = if settings.Parallel then Array.Parallel.mapi else Array.mapi
    let info = splitConstraints polInfo
    let (aggInfo, _, _) = info
    let pairs = Array.ofList polInfo.Policy
    let timedConfigs, prefixTime =
        Profile.time (mapi (fun i x -> 
            Profile.time (compileForSinglePrefix fullName (i+1) polInfo aggInfo) x)) pairs
    let nAggFails = Array.map (fun (res,_) -> res.K) timedConfigs
    let k = Array.fold minFails None nAggFails
    let configs, times = Array.unzip timedConfigs
    let joined, joinTime = Profile.time (joinConfigs polInfo info) (Array.toList configs)
    let minJoined, minTime = 
        if settings.Minimize
        then Profile.time RouterWide.minimize joined
        else joined, int64 0
    let buildTimes = Array.map (fun c -> c.BuildTime) configs
    let minTimes = Array.map (fun c -> c.MinimizeTime) configs
    let orderTimes = Array.map (fun c -> c.OrderingTime) configs
    let genTimes = Array.map (fun c -> c.ConfigTime) configs
    let stats = 
        {NumPrefixes=Array.length configs;
         ConfigSize = size minJoined;
         PrefixTime = prefixTime;
         PerPrefixTimes = times
         PerPrefixBuildTimes = buildTimes;
         PerPrefixMinTimes = minTimes;
         PerPrefixOrderTimes = orderTimes;
         PerPrefixGenTimes = genTimes;
         JoinTime = joinTime;
         MinTime = minTime} in
    {Abgp = minJoined; AggSafety = k; Stats = stats}

/// Unit tests for compilation.
/// Hooks into the compileToIR function to test
/// compilation at the per-prefix level.
/// Checks valid import/export filters + preferences

module Test = 

    let isPeer (ain,aout) x m = 
        let check y = 
            match y with 
            | Any -> true
            | In -> Set.contains x ain
            | Router y -> x = y
            | Out -> Set.contains x aout

        match m with 
        | Peer y -> check y
        | State(_,y) -> check y
        | _ -> false

    let getPref (ain,aout) (x:string) dc = 
        match dc with 
        | Originate -> 100  // TODO
        | Filters fs ->
            let lp = List.tryFind (fun f -> 
                match f with 
                | Deny -> false 
                | Allow ((m,_),_) -> isPeer (ain,aout) x m) fs
            match lp with
            | Some (Allow ((_,lp), _)) -> lp
            | _ -> 100

    let prefersPeer (ain,aout) (_,config) x (a,b) =
        try 
            let deviceConfig = Map.find x config 
            let lp1 = getPref (ain,aout) a deviceConfig
            let lp2 = getPref (ain,aout) b deviceConfig
            lp1 > lp2
        with _ -> false

    let receiveFrom (ain,aout) (_,config) x y = 
        let actions = Map.find x config
        match actions with 
        | Originate -> false 
        | Filters fs ->
            List.exists (fun f -> 
                match f with 
                | Deny -> false 
                | Allow ((m,_), _) -> isPeer (ain,aout) y m) fs

    let originates (_, config) x =
        let actions = Map.find x config
        match actions with 
        | Originate -> true
        | Filters _ -> false

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
        let res = compileToIR "" 0 (Predicate.prefix (10u, 0u, 0u, 0u) 32u) None aggs reb [reb.Build pol]
        match res with
        | Err _ -> failed ()
        | Ok(res) -> 
            match res.K with 
            | Some x when x.NumFailures = 1 -> passed ()
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
            logInfo(0, "\n" + msg)
            let (ain, aout) = Topology.alphabet test.Topo
            let ain = ain |> Set.map (fun v -> v.Loc)
            let aout = aout |> Set.map (fun v -> v.Loc)
            let reb = Regex.REBuilder(test.Topo)
            let built = test.Rf reb
            if not (Topology.isWellFormed test.Topo) then 
                failed ()
                logInfo(0, msg)
            else
                let pred = Predicate.top
                match compileToIR (settings.DebugDir + test.Name) 0 pred None Map.empty reb built with 
                | Err(x) ->
                    if (Option.isSome test.Receive || 
                        Option.isSome test.Originate || 
                        Option.isSome test.Prefs || 
                        Option.isNone test.Fail) then 
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, should compile but did not" test.Name
                        logInfo(0, msg)
                    match test.Fail, x with 
                    | Some FRNoPathForRouters, NoPathForRouters _ -> passed ()
                    | Some FRInconsistentPrefs, InconsistentPrefs _ -> passed ()
                    | Some FRCantControlPeers, UncontrollableEnter _ -> passed ()
                    | Some FRCantControlPeers, UncontrollablePeerPreference _ -> passed ()
                    | _ ->
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, expected error" test.Name
                        logInfo(0, msg)
                | Ok(res) -> 
                    let config = res.Config
                    if (Option.isNone test.Receive || 
                        Option.isNone test.Originate || 
                        Option.isNone test.Prefs || 
                        Option.isSome test.Fail) then
                        failed ()
                        let msg = sprintf "[Failed]: Name: %s, should not compile but did" test.Name
                        logInfo(0, msg)
                    else
                        let mutable fail = false
                        let rs = Option.get test.Receive
                        for (x,y) in rs do 
                            if not (receiveFrom (ain,aout) config x y) then
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should receive from %s but did not" test.Name x y
                                logInfo(0, msg)
                        let os = Option.get test.Originate
                        for x in os do 
                            if not (originates config x) then 
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should originate a route but did not" test.Name x
                                logInfo(0, msg)
                        let ps = Option.get test.Prefs
                        for (x,a,b) in ps do
                            if not (prefersPeer (ain,aout) config x (a,b)) then 
                                fail <- true
                                let msg = sprintf "[Failed]: (%s) - %s should prefer %s to %s but did not" test.Name x a b
                                logInfo(0, msg)
                        if fail 
                        then failed ()
                        else passed ()
        printfn "%s" border

    let run () =
        testAggregationFailure ()
        testCompilation ()