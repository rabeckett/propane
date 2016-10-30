module Abgp

open CGraph
open System.Collections.Generic
open Util
open Util.Debug
open Util.Error
open Util.Format

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
   override x.ToString() = 
      match x with
      | Peer p -> "peer=" + (string p)
      | State(c, p) -> sprintf "peer=%s, comm=%s" (string p) c
      | PathRE r -> sprintf "regex('%s')" (string r)

type Modification = 
   | SetComm of string
   | SetMed of int
   | PrependPath of int
   override this.ToString() = 
      match this with
      | SetComm(is) -> sprintf "comm<-%s" is
      | SetMed i -> sprintf "med<-%d" i
      | PrependPath i -> sprintf "prepend %d" i

type Import = Match * LocalPref

type Export = Peer * Modification list

type Filter = 
   | Deny
   | Allow of Import * Export list

type Actions = 
   | Filters of Export list option * Filter list

type DeviceAggregates = Set<Route.Prefix * string list>

type DeviceTags = Set<(string * Route.Prefix list) * string list>

type DeviceMaxRoutes = Set<int * string list>

type DeviceControl = 
   { Aggregates : DeviceAggregates
     Tags : DeviceTags
     MaxRoutes : DeviceMaxRoutes }

type PredMatch = 
   | Pred of Route.Predicate
   | Comm of Route.Predicate * string

type RouterConfig = 
   { Actions : (PredMatch * Actions) list
     Control : DeviceControl }

type T = 
   { PolInfo : Ast.PolInfo
     RConfigs : Map<string, RouterConfig> }

type CounterExample = 
   | InconsistentPrefs of CgState * CgState * Consistency.Explanation
   | UncontrollableEnter of string
   | UncontrollablePeerPreference of string

/// Functions that operate over final per-router 
/// configurations, and display them in a nice, 
/// human-readable format w/indentation.
open Core.Printf

let lookupRouter (pi : Ast.PolInfo) s = 
   let ti = pi.Ast.TopoInfo
   Topology.router s ti

let lookupPeer pi p = 
   match p with
   | Router s -> Router(lookupRouter pi s)
   | _ -> p

let lookupMatch pi m = 
   match m with
   | Peer(p) -> Peer(lookupPeer pi p)
   | State(c, p) -> State(c, lookupPeer pi p)
   | PathRE _ -> m

let formatExport pi sb peer acts = 
   let actStr = 
      if acts <> [] then ", " + Util.List.joinBy ", " (List.map string acts)
      else ""
   
   let peerStr = 
      peer
      |> lookupPeer pi
      |> string
   
   bprintf sb "[export peer<-%s%s]" peerStr actStr

let formatActions (sb : System.Text.StringBuilder) (pi : Ast.PolInfo) pred (actions : Actions) = 
   let origStr, predStr = 
      match pred with
      | Pred p -> 
         let s = Route.toString p
         s, s
      | Comm(p, c) -> Route.toString p, "comm=" + c
   match actions with
   | Filters(o, fs) -> 
      match o with
      | None -> ()
      | Some es -> 
         bprintf sb "\n  origin:  %s " origStr
         match es with
         | [ (peer, acts) ] -> formatExport pi sb peer acts
         | _ -> 
            for (peer, acts) in es do
               bprintf sb "\n             "
               formatExport pi sb peer acts
      for f in fs do
         match f with
         | Deny -> 
            bprintf sb "\n  deny:    "
            bprintf sb "%s" predStr
         | Allow((m, lp), es) -> 
            let allow = 
               m
               |> lookupMatch pi
               |> string
            bprintf sb "\n  allow:   %s, %s " predStr allow
            match es with
            | [ (peer, acts) ] -> 
               formatExport pi sb peer acts
               if lp <> 100 then bprintf sb " (lp=%d) " lp
            | _ -> 
               if lp <> 100 then bprintf sb "(lp=%d) " lp
               for (peer, acts) in es do
                  bprintf sb "\n             "
                  formatExport pi sb peer acts

let formatPred (polInfo : Ast.PolInfo) (pred : PredMatch) (racts : Map<string, Actions>) = 
   let sb = System.Text.StringBuilder()
   Map.iter (fun (router : string) act -> 
      bprintf sb "\nRouter %s" router
      formatActions sb polInfo pred act) racts
   sb.ToString()

let format (config : T) = 
   let sb = System.Text.StringBuilder()
   for kv in config.RConfigs do
      let ti = config.PolInfo.Ast.TopoInfo
      let routerName = Topology.router kv.Key ti
      let routerConfig = kv.Value
      bprintf sb "Router %s" routerName
      for (prefix, peers) in routerConfig.Control.Aggregates do
         for peer in peers do
            bprintf sb "\n  control: aggregate(%s, %s)" (string prefix) (Topology.router peer ti)
      for ((c, prefix), peers) in routerConfig.Control.Tags do
         for peer in peers do
            bprintf sb "\n  control: tag(%s, %s, %s)" c (string prefix) (Topology.router peer ti)
      for (i, peers) in routerConfig.Control.MaxRoutes do
         for peer in peers do
            bprintf sb "\n  control: max_routes(%d)" i
      for (pred, actions) in routerConfig.Actions do
         formatActions sb config.PolInfo pred actions
      bprintf sb "\n\n"
   sb.ToString()

/// Helper functions to make changes to the configuration
/// either by modifying or removing a part of the configuration.
module Update = 
   let updateConfig f (config : T) = 
      let rconfigs = Map.map f config.RConfigs
      { PolInfo = config.PolInfo
        RConfigs = rconfigs }
   
   let updateActions f (config : T) = 
      updateConfig (fun router rconf -> 
         let actions = List.choose f rconf.Actions
         { Actions = actions
           Control = rconf.Control }) config
   
   let updateFilter f (config : T) = 
      updateActions (fun (pred, acts) -> 
         match acts with
         | Filters(o, fs) -> Some(pred, (Filters(o, List.choose (f pred) fs)))) config
   
   let updateOrigins f (config : T) = 
      updateActions (fun (pred, acts) -> 
         match acts with
         | Filters(o, fs) -> 
            let o' = 
               match o with
               | None -> o
               | Some es -> Some(List.choose (f pred) es)
            Some(pred, Filters(o', fs))) config
   
   let updateAllow f (config : T) = 
      updateFilter (fun pred filt -> 
         match filt with
         | Deny -> Some Deny
         | Allow((m, lp), es) as v -> 
            match f pred m lp es with
            | None -> None
            | Some(m', lp', es') -> Some(Allow((m', lp'), es'))) config
   
   let updateMods f (config : T) = 
      let config = updateOrigins (fun pred e -> f pred (Match.Peer(Router "")) -1 e) config
      updateAllow (fun pred m lp es -> 
         let es' = List.choose (f pred m lp) es
         Some(m, lp, es')) config
   
   let updateMod f (config : T) = 
      updateMods (fun pred m lp (peer, mods) -> Some(peer, List.choose (f pred m lp peer) mods)) 
         config

/// Minimization helper functions for reducing the size of 
/// configurations. Minimization occurs at 3 levels of granularity:
/// 1. Per product graph node - used during generation for memory efficiency
/// 2. Per prefix - allows us to minimize based on an equivalence class
/// 3. Per router - minimize for a router (e.g., fall-through elimination)
let size (config : T) = 
   let mutable count = 0
   for kv in config.RConfigs do
      let rconf = kv.Value
      for (_, actions) in rconf.Actions do
         match actions with
         | Filters(_, fs) -> 
            for f in fs do
               match f with
               | Deny -> count <- count + 1
               | Allow((m, _), es) -> count <- count + (List.length es)
   count

let private chooseAction f (filt : Filter) = 
   let inline adjActs (peer, acts) = peer, List.choose f acts
   match filt with
   | Deny -> Deny
   | Allow((m, lp), es) -> Allow((m, lp), List.map adjActs es)

module NodeWide = 
   let removeRedundantTag m exports = 
      match m with
      | Match.Peer _ | Match.PathRE _ -> exports
      | Match.State(is, _) -> 
         List.choose (fun (peer, acts) -> 
            let acts' = 
               List.filter (fun a -> 
                  match a with
                  | SetComm c -> c <> is
                  | _ -> true) acts
            Some(peer, acts')) exports
   
   let removeCommMatchForUnqEdges cg (eCounts : Dictionary<_, _>) v m = 
      let inline unq e = 
         let b, res = eCounts.TryGetValue(e)
         b && res = 1
      match m with
      | Match.State(c, peers) -> 
         match peers with
         | Any | In -> 
            let ins = CGraph.neighborsIn cg v |> Seq.map CGraph.loc
            if Seq.forall (fun i -> unq (i, v.Node.Loc)) ins then Match.Peer(peers)
            else m
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
   let private allCommMatches filters = 
      Util.List.fold (fun acc f -> 
         match f with
         | Allow((Match.State(c, _), _), es) -> Set.add c acc
         | _ -> acc) Set.empty filters
   
   let private tags es = 
      List.fold (fun acc (peer, mods) -> 
         List.fold (fun acc m -> 
            match m with
            | SetComm c -> Set.add c acc
            | _ -> acc) acc mods) Set.empty es
   
   let private allCommTags filters origins = 
      let fromFilters = 
         Util.List.fold (fun acc f -> 
            match f with
            | Allow((_, _), es) -> Set.union acc (tags es)
            | _ -> acc) Set.empty filters
      Set.union (tags origins) fromFilters
   
   let updateActions usedMatches _ actions = 
      match actions with
      (* | Originate es -> 
         let es' = 
            List.map (fun (peer, acts) -> 
               let acts' = 
                  List.filter (fun a -> 
                     match a with
                     | SetComm(c) when c <> "no-export" && not (Set.contains c usedMatches) -> false
                     | _ -> true) acts
               (peer, acts')) es
         Originate es' *)
      | Filters(o, fs) -> 
         let o' = 
            match o with
            | None -> o
            | Some es -> 
               let es' = 
                  List.map (fun (peer, acts) -> 
                     let acts' = 
                        List.filter (fun a -> 
                           match a with
                           | SetComm(c) when c <> "no-export" && not (Set.contains c usedMatches) -> 
                              false
                           | _ -> true) acts
                     (peer, acts')) es
               Some es'
         
         let fs' = 
            List.map (fun f -> 
               chooseAction (fun a -> 
                  match a with
                  | SetComm(c) when c <> "no-export" && not (Set.contains c usedMatches) -> None
                  | _ -> Some a) f) fs
         
         Filters(o', fs')
   
   let updateMatches usedTags _ actions = 
      match actions with
      | Filters(o, fs) -> 
         let fs' = 
            List.choose (fun f -> 
               match f with
               | Allow((Match.State(c, _), _), _) when c <> "no-export" 
                                                       && not (Set.contains c usedTags) -> None
               | _ -> Some f) fs
         Filters(o, fs')
   
   let private removeUnobservedTags (config : Map<string, Actions>) = 
      let allFilters = 
         config |> Map.fold (fun acc router actions -> 
                      match actions with
                      | Filters(o, fs) -> acc @ fs) []
      
      let usedMatches = allCommMatches allFilters
      Map.map (updateActions usedMatches) config
   
   let private removeUnobservedMatches (config : Map<string, Actions>) = 
      let allFilters, allOrigins = 
         config |> Map.fold (fun (accl, accr) router actions -> 
                      match actions with
                      | Filters(o, fs) -> 
                         match o with
                         | None -> (accl @ fs, accr)
                         | Some es -> (accl @ fs, accr @ es)) ([], [])
      
      let usedTags = allCommTags allFilters allOrigins
      Map.map (updateMatches usedTags) config
   
   (*
   let removeUnreachableRules (cg : CGraph.T) (config : Map<string, Actions>) = 
      let inline anyValue f vs = 
         if Set.isEmpty vs then Route.pb.True
         else Set.fold (fun x y -> Route.pb.Or(x, f y)) Route.pb.False vs
      
      let allFilters, allOrigins = 
         Map.fold (fun (accl, accr) r acts -> 
            match acts with
            | Filters(o, fs) -> 
               match o with
               | None -> (Seq.append accl fs, accr)
               | Some es -> (Seq.append accl fs, Seq.append accr es)) (Seq.empty, Seq.empty) config
      
      let comms = allCommTags allFilters allOrigins
      let anyComm = anyValue Route.pb.Community comms
      
      let aux router actions = 
         let nodes = Topology.vertices cg.Topo |> Seq.filter (fun x -> x.Loc = router)
         let peers = Seq.collect (Topology.neighbors cg.Topo) nodes
         
         let peersIn = 
            peers
            |> Seq.filter Topology.isInside
            |> Seq.map (fun x -> x.Loc)
            |> Set.ofSeq
         
         let peersOut = 
            peers
            |> Seq.filter Topology.isOutside
            |> Seq.map (fun x -> x.Loc)
            |> Set.ofSeq
         
         let peers = Set.union peersIn peersOut
         let anyPeer = anyValue Route.pb.Location peers
         let anyInPeer = anyValue Route.pb.Location peersIn
         let anyOutPeer = anyValue Route.pb.Location peersOut
         
         let predOfPeer (p : Peer) = 
            match p with
            | Router r -> Route.pb.Location r
            | In -> anyInPeer
            | Out -> anyOutPeer
            | Any -> anyPeer
         match actions with
         | Filters(o, fs) -> 
            /// Start with the filler predicate to encode the finite domain of communities, locations
            let acc = ref (Route.pb.Not(Route.pb.And(anyComm, anyPeer)))
            
            let fs' = 
               List.choose (fun f -> 
                  let oldAcc = !acc
                  let mutable isRE = false
                  match f with
                  | Deny -> acc := Route.pb.True
                  | Allow((m, _), _) -> 
                     let p = 
                        match m with
                        | Peer p -> predOfPeer p
                        | State(c, p) -> Route.pb.And(Route.pb.Community c, predOfPeer p)
                        | PathRE _ -> 
                           isRE <- true
                           Route.pb.False
                     acc := Route.pb.Or(!acc, p)
                  if !acc <> oldAcc || isRE then Some f
                  else None) fs
            
            Filters(o, fs')
      Map.map aux config *)
   let minimize (cg : CGraph.T) (config : Map<string, Actions>) = 
      config
      |> removeUnobservedTags
      |> removeUnobservedMatches

(* removeUnreachableRules cg *)

module RouterWide = 
   type CoverResult = 
      | No
      | Yes
      | ForComm of string
   
   let disjointMatch m1 m2 = 
      match m1, m2 with
      | Peer(x), Peer(y) | State(_, x), Peer(y) -> 
         match x, y with
         | Any, _ -> false
         | _, Any -> false
         | In, Out | Out, In -> true
         | _, _ -> false
      | _, _ -> true
   
   let disjointFilter (f1 : Filter) (f2 : Filter) = 
      match f1, f2 with
      | Deny, _ -> false
      | _, Deny -> false
      | Allow((m1, _), es1), Allow((m2, _), es2) -> disjointMatch m1 m2
   
   let coveringPeer x y = 
      match x, y with
      | _, Any -> true
      | In, In | Out, Out -> true
      | _, _ -> false // TODO: use topology for routers
   
   let coveringMatch m1 m2 = 
      match m1, m2 with
      | Peer x, Peer y -> 
         if coveringPeer x y then Yes
         else No
      | Peer x, State(d, y) -> 
         if coveringPeer x y then ForComm d
         else No
      | State(_, x), Peer y -> 
         if coveringPeer x y then Yes
         else No
      | State(c, x), State(d, y) -> 
         if c = d && coveringPeer x y then ForComm d
         else No
      | _, _ -> No
   
   let coveringFilter (f1 : Filter) (f2 : Filter) = 
      match f1, f2 with
      | Deny, Deny -> Yes
      | Deny, Allow _ -> No
      | Allow _, Deny -> Yes
      | Allow((m1, _), _), Allow((m2, _), _) -> coveringMatch m1 m2
   
   let eqExports (f1 : Filter) (f2 : Filter) = 
      match f1, f2 with
      | Deny, Deny -> true
      | Deny, _ -> false
      | _, Deny -> false
      | Allow(_, es1), Allow(_, es2) -> es1 = es2
   
   let makePairs (pairs : (_ * Actions) list) = 
      let mutable res = []
      for (pred, actions) in pairs do
         match actions with
         | Filters(_, fs) -> 
            for f in fs do
               res <- (pred, f) :: res
      List.rev res
   
   let inline unMakePairs pairs = 
      Seq.groupBy fst pairs
      |> Seq.map (fun (x, ys) -> (x, Seq.map snd ys |> List.ofSeq))
      |> List.ofSeq
   
   let addBackPair origins pairs acc (p, actions) = 
      if Map.containsKey p origins then (p, origins.[p]) :: acc
      else 
         match Map.tryFind p pairs with
         | None -> acc
         | Some fs -> (p, Filters fs) :: acc
   
   let inline addOriginator acc (p, actions) = 
      match actions with
      | Filters(Some _, _) -> Map.add p actions acc
      | _ -> acc
   
   let rec catchAll allComms covered (p1, f1) below : bool = 
      match below with
      | [] -> false
      | ((p2, f2) as pair) :: tl -> 
         if disjointFilter f1 f2 && Route.pb.And(p1, p2) = Route.pb.False then 
            catchAll allComms covered (p1, f1) tl
         else 
            match coveringFilter f1 f2 with
            | No -> catchAll allComms covered (p1, f1) tl
            | Yes -> 
               if Route.pb.Implies(p1, p2) then eqExports f1 f2
               else catchAll allComms covered (p1, f1) tl
            | ForComm c -> 
               let newCovered = Set.add c covered
               if newCovered = allComms then true
               else catchAll allComms newCovered (p1, f1) tl
   
   let rec fteAux allComms pairs = 
      match pairs with
      | [] -> []
      | ((p1, f1) as pair) :: tl -> 
         let tl = fteAux allComms tl
         if catchAll allComms Set.empty pair tl then tl
         else pair :: tl
   
   let getAllCommunities (config : T) = 
      let comms = ref Set.empty
      Update.updateMod (fun _ _ _ _ m -> 
         match m with
         | SetComm c -> 
            comms := Set.add c !comms
            Some m
         | _ -> Some m) config
      |> ignore
      !comms
   
   (* let fallThroughElimination allComms (rconfig : RouterConfig) : RouterConfig = 
      let usesTemplate = 
         List.fold (fun acc (p, _) -> acc || Route.isTemplate p) false rconfig.Actions
      if usesTemplate then rconfig
      else 
         let actions = List.map (fun (p, x) -> (Route.getConcrete p, x)) rconfig.Actions
         let origins = Util.List.fold addOriginator Map.empty rconfig.Actions
         
         let pairs = 
            fteAux allComms (makePairs actions)
            |> unMakePairs
            |> List.map (fun (p, x) -> (Route.ConcretePred(p), x))
            |> Map.ofList
         
         let actions = Util.List.fold (addBackPair origins pairs) [] rconfig.Actions
         { Control = rconfig.Control
           Actions = List.rev actions } 
   
   let minimizeForRouter (pi : Ast.PolInfo) allComms r rconf = 
      rconf |> fallThroughElimination allComms *)
   // TODO: reevaluate this function for soundness
   // Perform fallthrough elimination on route maps
   let getBorderRouters (ti : Topology.TopoInfo) = 
      let ext = ti.SelectGraphInfo.ExternalNames
      let g = ti.SelectGraphInfo.Graph
      let mutable acc = Set.empty
      for (x, y) in Topology.edges g do
         if ext.Contains(y.Loc) then acc <- Set.add x.Loc acc
      acc
   
   let removeSpecificMatchWhenInternal (config : T) (ti : Topology.TopoInfo) = 
      let borders = getBorderRouters ti
      
      let rconfigs = 
         Map.map (fun name rconf -> 
            if not <| borders.Contains(name) then 
               let mutable matches = Set.empty
               let mutable actions = Set.empty
               let mutable origins = Set.empty
               
               let butLast = 
                  match List.rev rconf.Actions with
                  | [] -> []
                  | hd :: tl -> List.rev tl
               for (pred, Filters(orig, filts)) in butLast do
                  match orig with
                  | None -> ()
                  | Some es -> origins <- Set.add (pred, es) origins
                  for f in filts do
                     match f with
                     | Deny -> matches <- Set.add None matches
                     | Allow((m, lp), acts) -> 
                        matches <- Set.add (Some(m, lp)) matches
                        actions <- Set.add acts actions
               if Set.count matches = 1 && Set.count actions <= 1 then 
                  let m = Set.minElement matches
                  let a = Set.minElement actions
                  
                  let filt = 
                     match m with
                     | None -> Deny
                     | Some(m, lp) -> Allow((m, lp), a)
                  
                  let actions = 
                     origins
                     |> Set.toList
                     |> List.map (fun (pred, es) -> (pred, Filters(Some es, [])))
                  
                  let actions = actions @ [ (Pred Route.top, Filters(None, [ filt ])) ]
                  { rconf with Actions = actions }
               else rconf
            else rconf) config.RConfigs
      { config with RConfigs = rconfigs }
   
   let minimize (config : T) (ti : Topology.TopoInfo) = config // removeSpecificMatchWhenInternal config ti

(*
    let settings = Args.getSettings()
    let allComms = getAllCommunities config
    
    let map = 
      if settings.Parallel then Array.Parallel.map
      else Array.map
    
    let rconfs = 
      config.RConfigs
      |> Map.toArray
      |> map (fun (r, rconf) -> r, minimizeForRouter config.PolInfo allComms r rconf)
      |> Map.ofArray
    
    { PolInfo = config.PolInfo
      RConfigs = rconfs } *)

/// Compilation from CGraph (product graph) to a complete IR config.
/// Compiles for each prefix in parallel and then merges the results
/// together, sequentially, to get a per-router configuration.
/// Minimization per-router the occurs in parallel.
type AggregationSafety = 
   { NumFailures : int
     PrefixLoc : string
     AggregateLoc : string
     Prefix : Route.Prefix
     Aggregate : Route.Prefix }

type PredConfig = Route.Predicate * Map<string, Actions>

type PrefixResult = 
   { K : AggregationSafety option
     BuildTime : int64
     MinimizeTime : int64
     AggAnalysisTime : int64
     OrderingTime : int64
     InboundAnalysisTime : int64
     ConfigTime : int64
     Config : PredConfig }

type PrefixCompileResult = Result<PrefixResult, CounterExample>

type Stats = 
   { NumPrefixes : int
     ConfigSize : int
     PrefixTime : int64
     PerPrefixTimes : int64 array
     PerPrefixBuildTimes : int64 array
     PerPrefixMinTimes : int64 array
     PerPrefixAggAnalysisTimes : int64 array
     PerPrefixOrderTimes : int64 array
     PerPrefixInboundTimes : int64 array
     PerPrefixGenTimes : int64 array
     JoinTime : int64
     MinTime : int64 }

type CompilationResult = 
   { Abgp : T
     AggSafety : AggregationSafety option
     Stats : Stats }

let joinConfigs polInfo (aggs, comms, maxroutes) (results : PrefixResult list) : T = 
   let mutable result = Map.empty
   for v in results do
      let (pred, config) = v.Config
      for kv in config do
         let dc = kv.Value
         let router = kv.Key
         let value = (Pred pred, dc)
         match Map.tryFind router result with
         | None -> result <- Map.add router [ value ] result
         | Some x -> result <- Map.add router (value :: x) result
   let routerConfigs = 
      Map.map (fun router vs -> 
         let a = Util.Map.getOrDefault router Set.empty aggs
         let b = Util.Map.getOrDefault router Set.empty comms
         let c = Util.Map.getOrDefault router Set.empty maxroutes
         { Actions = List.rev vs
           Control = 
              { Aggregates = a
                Tags = b
                MaxRoutes = c } }) result
   { PolInfo = polInfo
     RConfigs = routerConfigs }

/// Ensure well-formedness for controlling 
/// traffic entering the network. MED and prepending allow
/// certain patterns of control to immediate neighbors only
module Incoming = 
   type IncomingPattern = 
      | Anything
      | Nothing of string
      | Specific of CGraph.CgState
   
   type IncomingInfo = 
      { Peers : seq<CgState>
        Info : Map<CgState, IncomingPattern> }
   
   type IncomingExportMap = Map<CgState, Modification list>
   
   let collectForPeer cg acc peer = 
      let reachable = Reachable.dfs cg peer Down
      let reach = ResizeArray()
      let routs = CGraph.repeatedOuts cg
      let mutable hasRepeatedOut = false
      for v in reachable do
         if v <> peer && Topology.isTopoNode v.Node then 
            reach.Add(v)
            if routs.Contains v then hasRepeatedOut <- true
      (* if routs.Contains peer then hasRepeatedOut <- true *)
      let hasOther = (reach.Count > 1) || (not hasRepeatedOut && reach.Count > 0)
      match hasRepeatedOut, hasOther with
      | false, false -> Map.add peer (Nothing peer.Node.Loc) acc
      | true, false -> Map.add peer Anything acc
      | _, true -> Map.add peer (Specific peer) acc
   
   let collectIncomingInfo (cg : CGraph.T) : IncomingInfo = 
      // TODO: This is pretty inefficient
      let isExportPeer v = 
         Topology.isOutside v.Node 
         && Seq.exists (fun u -> Topology.isInside u.Node) (neighborsIn cg v)
      let exportPeers = Seq.filter isExportPeer cg.Graph.Vertices
      let info = Seq.fold (collectForPeer cg) Map.empty exportPeers
      { Peers = exportPeers
        Info = info }
   
   let addExports (settings : Args.T) info peers actions exportMap = 
      let mutable actions = actions
      for p in peers do
         match Map.find p info.Info with
         | Anything -> ()
         | Nothing x -> 
            if settings.UseNoExport then actions <- (SetComm "no-export") :: actions
            else raise (UncontrollableEnterException x)
         | Specific _ -> raise (UncontrollableEnterException p.Node.Loc)
         exportMap := Map.add p actions !exportMap
      !exportMap
   
   let getUnique peers = Set.ofSeq (Seq.map (fun p -> p.Node.Loc) peers)
   
   let getMostPreference f g (cg : CGraph.T) (x : CgState) = 
      x
      |> CGraph.neighborsIn cg
      |> Seq.filter CGraph.isInside
      |> Seq.map g
      |> f
   
   let getSeen cg (peers : seq<CgState>) = 
      let mutable pairs = Set.empty
      let mutable nodes = Set.empty
      for p in peers do
         nodes <- Set.add p.Node.Loc nodes
         let nin = CGraph.neighborsIn cg p
         for n in nin do
            if CGraph.isInside n then pairs <- Set.add (n.Node.Loc, p.Node.Loc) pairs
      (nodes, pairs)
   
   let configureIncomingTraffic cg (ord : Consistency.Ordering) : IncomingExportMap = 
      let settings = Args.getSettings()
      let info = collectIncomingInfo cg
      let preferences = Seq.map (fun p -> (p, Reachable.srcAccepting cg p Down)) info.Peers
      // ensure all preferences align
      for (p, prefs) in preferences do
         if Set.count prefs > 1 then 
            let x = Set.minElement prefs
            let y = Set.maxElement prefs
            error 
               (sprintf "Multiple conflicting preferences (%d and %d) for peer %s" x y p.Node.Loc)
      // Sort by most preferred entry point
      let byPreference = 
         preferences
         |> Seq.map (fun (p, prefs) -> (Set.minElement prefs, p))
         |> Seq.groupBy fst
         |> Seq.map (fun (x, y) -> (x, Seq.map snd y))
         |> Seq.sortBy fst
      
      // Collect inferred preferences as map
      let prefMap = Dictionary()
      for kv in ord do
         let mutable j = 0
         for x in kv.Value do
            j <- j + 1
            prefMap.[x] <- j
      // Collect worst case pref for each peer
      let mutable worstCasePrefs = Map.empty
      let mutable bestCasePrefs = Map.empty
      let mutable inferredWorstPrefs = Map.empty
      let mutable inferredBestPrefs = Map.empty
      let f1 = (fun v -> v.Accept)
      let f2 = (fun v -> prefMap.[v])
      for p in info.Peers do
         worstCasePrefs <- Map.add p (getMostPreference Seq.max f1 cg p) worstCasePrefs
         bestCasePrefs <- Map.add p (getMostPreference Seq.min f1 cg p) bestCasePrefs
         inferredWorstPrefs <- Map.add p (getMostPreference Seq.max f2 cg p) inferredWorstPrefs
         inferredBestPrefs <- Map.add p (getMostPreference Seq.min f2 cg p) inferredBestPrefs
      // Run through each preference level and check for safe preference
      let exportMap = ref Map.empty
      let mutable i = 0
      let mutable prev = None
      let mutable seenPairs = Set.empty
      let mutable seenPeers = Set.empty
      for (_, peers) in byPreference do
         match prev with
         | None -> exportMap := addExports settings info peers [] exportMap
         | Some ps -> 
            for p1 in peers do
               let nin = 
                  CGraph.neighborsIn cg p1
                  |> Set.ofSeq
                  |> Set.map (fun v -> (v.Node.Loc, p1.Node.Loc))
               // do we need meds or prepending?
               if not (settings.UseMed || settings.UsePrepending) then 
                  if seenPeers.Contains p1.Node.Loc then 
                     if not (Set.isSuperset seenPairs nin) then 
                        // let newPair = 
                        //   Seq.tryFind (fun v -> not <| seenPairs.Contains(v, p1.Node.Loc)) 
                        //      nin
                        //match newPair with
                        // | None -> ()
                        // | Some v -> 
                        raise (UncontrollablePeerPreferenceException p1.Node.Loc)
               for p2 in ps do
                  // preferences for different peers must be implementable from internal preferences
                  let p1Worse = bestCasePrefs.[p1] > worstCasePrefs.[p2]
                  let p1Worse = 
                     p1Worse 
                     || (loc p1 = loc p2 && inferredBestPrefs.[p1] > inferredWorstPrefs.[p2])
                  if not p1Worse then 
                     if p1.Node.Loc <> p2.Node.Loc then 
                        raise (UncontrollablePeerPreferenceException p1.Node.Loc)
            let mutable actions = []
            if settings.UsePrepending && i > 0 then actions <- (PrependPath(2 * i)) :: actions
            if settings.UseMed && i > 0 then actions <- (SetMed(80 + i)) :: actions
            exportMap := addExports settings info peers actions exportMap
         prev <- Some peers
         let (nodes, pairs) = getSeen cg peers
         seenPeers <- Set.union seenPeers nodes
         seenPairs <- Set.union seenPairs pairs
         i <- i + 1
      !exportMap

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
   
   let inline peerOnly cg (routs : Set<CgState>) nin = 
      let inline aux v = 
         let nin = neighborsIn cg v
         Set.contains v routs && Seq.length nin = 2 && Seq.exists ((=) cg.Start) nin
      Seq.length nin = 2 && Seq.exists ((=) cg.Start) nin && Seq.exists aux nin
   
   let getOutPeerType cg (routs : Set<CgState>) (x : CgState) = 
      if Topology.isOutside x.Node then 
         let nin = neighborsIn cg x
         if peerOnly cg routs nin then PeerMatch x
         else 
            printfn "construct regex"
            let re = CGraph.ToRegex.constructRegex cg x
            printfn "done"
            RegexMatch(re)
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
   let mutable matches = List.map (fun r -> Match.PathRE(Outgoing.getRegexMatch r)) regexes
   let mutable statesIn = Set.empty
   let mutable statesOut = Set.empty
   for v in peers do
      if Topology.isInside v.Node then statesIn <- Set.add v.State statesIn
      else statesOut <- Set.add v.State statesOut
   let states = Set.union statesIn statesOut
   // check if possible to compress matches
   if (states.Count = 1) && peerLocs = allPeers then 
      matches <- Match.State(string (Set.minElement states), Any) :: matches
      peers <- Set.empty
   else 
      if (statesIn.Count = 1) && peerLocsIn = inPeers && not peerLocsIn.IsEmpty then 
         matches <- Match.State(string (Set.minElement statesIn), In) :: matches
         peers <- Set.difference peers peersIn
      if (statesOut.Count = 1) && peerLocsOut = outPeers && not peerLocsOut.IsEmpty then 
         matches <- Match.Peer(Out) :: matches
         peers <- Set.difference peers peersOut
      for v in peers do
         let m = 
            if Topology.isInside v.Node then Match.State(string v.State, Router v.Node.Loc)
            else Match.Peer(Router v.Node.Loc)
         matches <- m :: matches
   matches

let inline getExport specialCase inExports x v = 
   if Topology.isOutside v.Node then 
      let ret = Map.find v inExports
      if ret <> [] then specialCase := true
      ret
   else [ SetComm(string x.State) ]

let getExports (allPeers, inPeers, outPeers) x inExports (outgoing : seq<CgState>) unqMatchPeer = 
   let toInside, toOutside = List.ofSeq outgoing |> List.partition CGraph.isInside
   
   let insideExport = 
      if toInside.Length > 0 then [ (In, [ SetComm(string x.State) ]) ]
      else []
   
   let specialCase = ref false
   let exports = 
      List.ofSeq toOutside 
      |> List.map (fun v -> (Router v.Node.Loc, getExport specialCase inExports x v))
   if not !specialCase then 
      let sendToLocs = Seq.map (fun v -> v.Node.Loc) outgoing |> Set.ofSeq
      
      let sendToLocs = 
         match unqMatchPeer with
         | Some x -> Set.add x sendToLocs
         | None -> sendToLocs
      if sendToLocs.IsSupersetOf outPeers then [ (Any, [ SetComm(string x.State) ]) ]
      else exports @ insideExport
   else exports @ insideExport

let inline edgeCounts (cg : CGraph.T) = 
   let counts = Dictionary()
   for e in cg.Graph.Edges do
      let key = (e.Source.Node.Loc, e.Target.Node.Loc)
      let mutable value = 0
      if counts.TryGetValue(key, &value) then counts.[key] <- value + 1
      else counts.Add(key, 1)
   counts

let getPeerInfo (vs : seq<Topology.Node>) = 
   let mutable allIn = Set.empty
   let mutable allOut = Set.empty
   for v in vs do
      if Topology.isInside v then 
         let r = v.Loc
         allIn <- Set.add r allIn
      else 
         if Topology.isOutside v then 
            let r = v.Loc
            allOut <- Set.add r allOut
   let all = Set.union allIn allOut
   (all, allIn, allOut)

let reindexPrefixCommunities (pc : PredConfig) : PredConfig = 
   let r = Util.Reindexer(HashIdentity.Structural)
   let (pred, conf) = pc
   pred, 
   Map.map (fun router acts -> 
      match acts with
      | Filters(o, fs) -> 
         Filters(o, 
                 List.map (fun f -> 
                    match f with
                    | Deny -> Deny
                    | Allow((m, lp), es) -> 
                       let m = 
                          match m with
                          | Match.State("no-export", p) -> m
                          | Match.State(c, p) -> Match.State(string (r.Index c), p)
                          | m -> m
                       
                       let es = 
                          List.map (fun (p, mods) -> 
                             let mods = 
                                List.map (fun m -> 
                                   match m with
                                   | SetComm "no-export" -> m
                                   | SetComm c -> SetComm(r.Index c |> string)
                                   | m -> m) mods
                             (p, mods)) es
                       
                       Allow((m, lp), es)) fs)) conf

let genConfig (cg : CGraph.T) (pred : Route.Predicate) (ord : Consistency.Ordering) 
    (inExports : Incoming.IncomingExportMap) : PredConfig = 
   let settings = Args.getSettings()
   if settings.CheckOnly then (pred, Map.empty)
   else 
      let ain, aout = Topology.alphabet cg.Topo
      let ain = ain |> Set.map (fun v -> v.Loc)
      let aout = aout |> Set.map (fun v -> v.Loc)
      let eCounts = edgeCounts cg
      let routs = CGraph.repeatedOuts cg
      let mutable config = Map.empty
      // generate a config for each internal router
      for router in ain do
         let mutable filters = []
         let mutable originates = false
         // look at the nodes according to preference
         let mutable prefs = Seq.empty
         // keep most recent exports for origination
         let mutable exports = []
         if ord.TryGetValue(router, &prefs) then 
            let mutable rules = []
            let mutable lp = 101
            for cgstate in prefs do
               lp <- lp - 1
               // get incoming and outgoing topology peer information
               let allInPeers = Topology.neighbors cg.Topo cgstate.Node
               let allOutPeers = Topology.neighbors cg.Topo cgstate.Node
               let inPeerInfo = getPeerInfo allInPeers
               let outPeerInfo = getPeerInfo allOutPeers
               let nsIn = neighborsIn cg cgstate
               let nsOut = neighbors cg cgstate
               // find who this node needs to send to and receive from
               let receiveFrom = Seq.filter CGraph.isRealNode nsIn
               let sendTo = Seq.filter CGraph.isRealNode nsOut
               // helps to minimize configuration
               let peerTypes = Seq.map (Outgoing.getOutPeerType cg routs) receiveFrom
               let origin = Seq.exists ((=) cg.Start) nsIn
               // get the compressed set of matches using *, in, out when possible
               let matches = getMatches inPeerInfo peerTypes
               
               // get the compressed set of exports taking into account if there is a unique receive peer
               let unqMatchPeer = 
                  match matches with
                  | [ Match.Peer(Router x) ] -> Some x
                  | [ Match.State(_, (Router x)) ] -> Some x
                  | _ -> None
               exports <- getExports outPeerInfo cgstate inExports sendTo unqMatchPeer
               // match/export local minimizations
               for m in matches do
                  let exps, m = 
                     if settings.Minimize then NodeWide.minimize cg eCounts cgstate m exports
                     else exports, m
                  filters <- Allow((m, lp), exps) :: filters
               originates <- origin || originates
         // no need for explicit deny if we allow everything
         match filters with
         | [ Allow((Match.Peer Any, _), _) ] -> ()
         | _ -> filters <- List.rev (Deny :: filters)
         // build the final configuration
         let o = 
            if originates then Some exports
            else None
         
         let deviceConf = Filters(o, filters)
         config <- Map.add router deviceConf config
      // prefix-wide minimizations
      config <- if settings.Minimize then PrefixWide.minimize cg config
                else config
      reindexPrefixCommunities (pred, config)

let inline insideLoc v = 
   if Topology.isInside v.Node then Some(v)
   else None

let inline insideOriginatorLoc v = 
   if (Topology.isInside v) && (Topology.canOriginateTraffic v) then Some v.Loc
   else None

let inline insideOriginators cg = 
   CGraph.neighbors cg cg.Start
   |> Seq.choose insideLoc
   |> Set.ofSeq

let warnNonExactOrigins cg pred = 
   let settings = Args.getSettings()
   if not settings.IsAbstract then 
      let originators = insideOriginators cg
      if originators.Count > 0 then 
         for tc in Route.trafficClassifiers pred do
            let (Route.TrafficClassifier(p, _)) = tc
            if not p.IsExact then 
               let msg = 
                  sprintf "Use of a range of prefixes: (%s) originated in your network. " (string p) 
                  + sprintf "This will default to the concrete prefix: %s" (string <| p.Example())
               warning msg

let abstractDisjointPathInfo (ti : Topology.TopoInfo) cg = 
   let inline aux acc o = Map.add o (AbstractAnalysis.reachability ti cg o) acc
   let originators = insideOriginators cg
   Set.fold aux Map.empty originators

let getLocsWithNoPath idx (ti : Topology.TopoInfo) cg (reb : Regex.REBuilder) dfas abstractPathInfo = 
   let settings = Args.getSettings()
   let startingLocs = 
      Array.fold (fun acc dfa -> Set.union (reb.StartingLocs dfa) acc) Set.empty dfas
   let originators = insideOriginators cg
   let originatorLocs = Set.map CGraph.loc originators
   
   let canSend = 
      Topology.vertices cg.Topo
      |> Seq.choose insideOriginatorLoc
      |> Set.ofSeq
   
   let needToSend = Set.intersect startingLocs canSend
   
   let locsThatNeedPath = 
      if settings.IsAbstract then needToSend
      else Set.difference needToSend originatorLocs
   
   let locsThatGetPath = 
      cg.Graph.Vertices
      |> Set.ofSeq
      |> Set.map loc
   
   // TODO: check reachability via abstract analysis
   logInfo (idx, sprintf "Locations that need path: %s" (locsThatNeedPath.ToString()))
   logInfo (idx, sprintf "Locations that get path: %s" (locsThatGetPath.ToString()))
   Set.difference locsThatNeedPath locsThatGetPath

let getUnusedPrefs cg res = 
   let mutable nRegexes = Set.empty
   let mutable i = 1
   for r in res do
      if r <> Regex.empty then nRegexes <- Set.add (int16 i) nRegexes
      i <- i + 1
   let prefs = CGraph.preferences cg
   Set.difference nRegexes prefs // don't use difference here

let warnAnycasts cg (polInfo : Ast.PolInfo) pred = 
   let settings = Args.getSettings()
   if (not settings.Anycast) then 
      let orig = insideOriginators cg |> Set.map CGraph.loc
      let ti = polInfo.Ast.TopoInfo
      if (Set.count orig > 0) && (settings.IsAbstract) && not (Route.isTemplate pred) then 
         let loc1 = orig.MinimumElement
         let loc1 = Topology.router loc1 ti
         let msg = 
            sprintf "Possibly anycasting from multiple locations for predicate %s. " 
               (Route.toString pred) 
            + sprintf "This is because traffic can end at abstract location %s, which " loc1 
            + sprintf "may correspond to multiple concrete locations."
         error msg
      else 
         if (Set.count orig > 1) then 
            let loc1 = orig.MinimumElement
            let loc2 = (Set.remove loc1 orig).MinimumElement
            let loc1 = Topology.router loc1 ti
            let loc2 = Topology.router loc2 ti
            let msg = 
               sprintf 
                  "The policy indicates that traffic can end up at multiple locations, e.g., %s and %s " 
                  loc1 loc2 
               + sprintf "for predicate %s. This will lead to anycasting the these prefixes. " 
                    (Route.toString pred) + sprintf "If you believe this is not a mistake, you can " 
               + sprintf "enable anycast by using the --anycast flag"
            error msg

let minDisjointPathsByLoc (abstractPathInfo : Map<CgState, AbstractAnalysis.AnalysisResult>) = 
   let inf = System.Int32.MaxValue
   let joinByMin acc k (all, some) = 
      Util.Map.adjust k.Node.Loc (inf, inf) (fun (a, s) -> (min a all, min s some)) acc
   let minByLoc acc k m = Map.add k.Node.Loc (Map.fold joinByMin Map.empty m) acc
   Map.fold minByLoc Map.empty abstractPathInfo

let getMinAggregateFailures (cg : CGraph.T) (pred : Route.Predicate) 
    (aggInfo : Map<string, DeviceAggregates>) 
    (abstractPathInfo : Map<CgState, AbstractAnalysis.AnalysisResult> option) = 
   let originators = insideOriginators cg
   let prefixes = Route.trafficClassifiers pred
   let smallest = ref System.Int32.MaxValue
   let pairs = ref None
   for (Route.TrafficClassifier(p, _)) in prefixes do
      Map.iter (fun aggRouter aggs -> 
         let relevantAggs = 
            Seq.choose (fun (aggPrefix, _) -> 
               if Route.isAggregateFor aggPrefix p then Some aggPrefix
               else None) aggs
         if not (Seq.isEmpty relevantAggs) then 
            let rAgg = Seq.head relevantAggs
            match abstractPathInfo with
            | Some info -> 
               // abstract analysis case
               let info = minDisjointPathsByLoc info
               for o in originators do
                  let x, y = o.Node.Loc, aggRouter
                  let (all, some) = info.[x].[y]
                  if all < !smallest then 
                     smallest := min !smallest (all - 1)
                     let p = (x, y, p, rAgg)
                     pairs := Some p
            | None -> 
               // concrete analysis case
               match CGraph.Failure.disconnectLocs cg originators aggRouter with
               | None -> ()
               | Some(k, x, y) -> 
                  if k < !smallest then 
                     smallest := min !smallest k
                     let p = (x, y, p, rAgg)
                     pairs := Some p) aggInfo
   if !smallest = System.Int32.MaxValue then None
   else 
      let (x, y, p, agg) = Option.get !pairs
      Some { NumFailures = !smallest
             PrefixLoc = x
             AggregateLoc = y
             Prefix = p
             Aggregate = agg }

let inline buildDfas (reb : Regex.REBuilder) res = List.map (fun r -> reb.MakeDFA(Regex.rev r)) res

let compileToIR idx pred (polInfo : Ast.PolInfo) aggInfo (reb : Regex.REBuilder) res : PrefixCompileResult = 
   let settings = Args.getSettings()
   let ti = polInfo.Ast.TopoInfo
   let name = sprintf "(%d)" idx
   let debugName = settings.DebugDir + File.sep + name
   let topo = reb.Topo()
   let dfas, dfaTime = Profile.time (buildDfas reb) res
   let dfas = Array.ofList dfas
   let cg, pgTime = Profile.time (CGraph.buildFromAutomata topo) dfas
   let buildTime = dfaTime + pgTime
   debug (fun () -> CGraph.generatePNG cg polInfo debugName)
   // minimize PG and record time
   let cg, minTime = Profile.time (CGraph.Minimize.minimize idx ti) cg
   debug (fun () -> CGraph.generatePNG cg polInfo (debugName + "-min"))
   // get the abstract reachability information
   let abstractPathInfo, at1 = 
      if settings.IsAbstract && not settings.Test (* && not (Map.isEmpty aggInfo) *) then 
         let v, t = Util.Profile.time (abstractDisjointPathInfo ti) cg
         Some(v), t
      else None, int64 0
   // warn for anycasts 
   if not settings.Test then 
      warnNonExactOrigins cg pred
      warnAnycasts cg polInfo pred
      // check if there is reachability
      // check there is a route for each location specified
      let lost = getLocsWithNoPath idx ti cg reb dfas abstractPathInfo
      if not (Set.isEmpty lost) then 
         let locs = Set.map (fun r -> Topology.router r ti) lost |> Util.Set.joinBy ", "
         let msg = sprintf "Routers: %s will have no path to the destination" locs
         warning msg
   // Find unused preferences for policies that were not drop
   let unusedPrefs = getUnusedPrefs cg res
   if not (Set.isEmpty unusedPrefs) then 
      let predStr = Route.toString pred
      Set.iter (fun i -> 
         let msg = sprintf "Unused preference %d policy " i + sprintf "for predicate %s" predStr
         warning msg) unusedPrefs
   try 
      // check aggregation failure consistency
      let k, at2 = 
         if settings.CheckFailures then 
            Util.Profile.time (getMinAggregateFailures cg pred aggInfo) abstractPathInfo
         else None, int64 0
      
      let aggAnalysisTime = at1 + at2
      // check that there is a valid ordering for BGP preferences to ensure compliance
      let (ordering, orderTime) = Profile.time (Consistency.findOrderingConservative idx) cg
      match ordering with
      | Ok ord -> 
         // check that BGP can ensure incoming traffic compliance
         let inExports, inboundTime = Util.Profile.time (Incoming.configureIncomingTraffic cg) ord
         let config, configTime = Profile.time (genConfig cg pred ord) inExports
         
         let result = 
            { K = k
              BuildTime = buildTime
              MinimizeTime = minTime
              AggAnalysisTime = aggAnalysisTime
              OrderingTime = orderTime
              InboundAnalysisTime = inboundTime
              ConfigTime = configTime
              Config = config }
         debug (fun () -> 
            let msg = formatPred polInfo (Pred pred) (snd config)
            System.IO.File.WriteAllText(sprintf "%s/%s.ir" settings.DebugDir name, msg))
         Ok(result)
      | Err((x, y, (ns, example))) -> Err(InconsistentPrefs(x, y, (ns, example)))
   with
      | UncontrollableEnterException s -> Err(UncontrollableEnter s)
      | UncontrollablePeerPreferenceException s -> Err(UncontrollablePeerPreference s)

let compileForSinglePrefix idx (polInfo : Ast.PolInfo) aggInfo (pred, reb, res) : PrefixResult = 
   match compileToIR idx pred polInfo aggInfo reb res with
   | Ok(config) -> config
   | Err(x) -> 
      let ti = polInfo.Ast.TopoInfo
      match x with
      | InconsistentPrefs(x, y, (ns, example)) -> 
         let l = Topology.router (CGraph.loc x) ti
         let msg = 
            sprintf "Cannot determine BGP local preferences that are correct under " 
            + sprintf "any failures for router %s for predicate %s. " l (Route.toString pred)
         
         let msg = 
            match example, ns with
            | None, _ | _, None -> msg
            | Some((x1, y1, path1), (x2, y2, path2)), Some(a, b) -> 
               let a = Topology.router a ti
               let b = Topology.router b ti
               let x1 = Topology.router x1.Node.Loc ti
               let y1 = Topology.router y1.Node.Loc ti
               let x2 = Topology.router x2.Node.Loc ti
               let y2 = Topology.router y2.Node.Loc ti
               let path1 = List.map (fun v -> Topology.router v.Node.Loc ti) path1
               let path2 = List.map (fun v -> Topology.router v.Node.Loc ti) path2
               let path1 = List.rev path1
               let path2 = List.rev path2
               let dst1 = List.head path1
               let dst2 = List.head path2
               let path1 = path1 |> Util.List.joinBy ","
               let path2 = path2 |> Util.List.joinBy ","
               msg 
               + (sprintf "Possible Issue: Suppose router %s prefers neighbor %s over %s. " l a b) 
               + (sprintf "However, %s might not be able to use " dst1) 
               + (sprintf "its preferred path %s, " path1) 
               + (sprintf "even though the path exists in the network. ") 
               + (sprintf "On the other hand, Suppose %s prefers neighbor %s over %s. " l b a) 
               + (sprintf "However, %s might not be able to use " dst2) 
               + (sprintf "its preferred path %s, " path2) 
               + (sprintf "even though the path exists in the network.")
         error msg
      | UncontrollableEnter x -> 
         let l = Topology.router x ti
         let msg = 
            sprintf "Cannot control inbound traffic from peer: %s for predicate %s. " l 
               (Route.toString pred) 
            + (sprintf 
                  "If you only want to allow traffic from neighbor %s (and not beyond), enable the --noexport flag." 
                  l)
         error msg
      | UncontrollablePeerPreference x -> 
         let l = Topology.router x ti
         let msg = 
            sprintf "Cannot control inbound preference from peer: %s for " l 
            + sprintf "predicate %s. Possibly enable prepending: --med or --prepending." 
                 (Route.toString pred)
         error msg

let checkAggregateLocs ins _ prefix links = 
   if Set.contains "out" ins then 
      let msg = 
         sprintf "Cannot aggregate on external location: " 
         + sprintf "out for prefix: %s" (string prefix)
      error msg
   match List.tryFind (fst >> Topology.isOutside) links with
   | None -> ()
   | Some x -> 
      let msg = 
         sprintf "Cannot aggregate on external location: " 
         + sprintf "%s for prefix: %s" (fst x).Loc (string prefix)
      error msg

let checkCommunityTagLocs ins _ (c, prefix) links = 
   if Set.contains "out" ins then 
      let msg = 
         sprintf "Cannot tag communities on external location: out " 
         + sprintf "for community %s, prefix: %s" c (string prefix)
      error msg
   match List.tryFind (fst >> Topology.isOutside) links with
   | None -> ()
   | Some x -> 
      let msg = 
         sprintf "Cannot tag communities on external location: " 
         + sprintf "%s for community %s prefix: %s" (fst x).Loc c (string prefix)
      error msg

let checkMaxRouteLocs ins outs i links = 
   let v = List.exists (fun (a, b) -> Topology.isOutside a && Topology.isOutside b) links
   let w = Set.contains "out" ins
   let x = Set.contains "out" outs
   let y = List.exists (fst >> Topology.isOutside) links
   let z = List.exists (snd >> Topology.isOutside) links
   if v || ((w || y) && (x || z)) then 
      let msg = 
         sprintf "Cannot set maxroutes(%d) on links " i 
         + sprintf "without an edge in the internal topology"
      error msg

let splitByLocation f topo (vs : _ list) = 
   let mutable acc = Map.empty
   for (k, ins, outs) in vs do
      let links = Topology.findLinks topo (ins, outs)
      f ins outs k links
      let pairs = 
         links
         |> List.map (fun (x, y) -> (x.Loc, y.Loc))
         |> Seq.groupBy fst
         |> Seq.map (fun (x, y) -> (x, Set.singleton (k, Seq.map snd y |> List.ofSeq)))
         |> Map.ofSeq
      acc <- Util.Map.merge acc pairs (fun _ (xs, ys) -> Set.union xs ys)
   acc

let splitConstraints (pi : Ast.PolInfo) = 
   let aggs, comms, maxroutes = 
      Util.List.fold (fun ((x, y, z) as acc) c -> 
         match c with
         | Ast.CAggregate(p, ins, outs) -> ((p, ins, outs) :: x, y, z)
         | Ast.CCommunity(s, p, ins, outs) -> (x, ((s, p), ins, outs) :: y, z)
         | Ast.CMaxRoutes(i, ins, outs) -> (x, y, (i, ins, outs) :: z)
         | _ -> acc) ([], [], []) pi.CConstraints
   
   let topo = pi.Ast.TopoInfo.SelectGraphInfo.Graph
   let aggInfo = splitByLocation checkAggregateLocs topo aggs
   let commInfo = splitByLocation checkCommunityTagLocs topo comms
   let maxRouteInfo = splitByLocation checkMaxRouteLocs topo maxroutes
   (aggInfo, commInfo, maxRouteInfo)

let minFails x y = 
   match x, y with
   | None, _ -> y
   | _, None -> x
   | Some a, Some b -> 
      if a.NumFailures < b.NumFailures then x
      else y

let reindexCommunities (config : T) : T = 
   let reindex = Util.Reindexer(HashIdentity.Structural)
   
   let changeModComms _ _ _ _ modif = 
      match modif with
      | SetComm(c) when c <> "no-export" -> 
         let c' = string (reindex.Index c)
         Some(SetComm c')
      | _ -> Some modif
   
   let changeMatchComms _ m lp es = 
      match m with
      | State(c, p) when c <> "no-export" -> 
         let c' = string (reindex.Index c)
         Some(Match.State(c', p), lp, es)
      | _ -> Some(m, lp, es)
   
   config
   |> Update.updateMod changeModComms
   |> Update.updateAllow changeMatchComms

let maxComm (config : T) = 
   let inline aux c = 
      try 
         Some(int c)
      with _ -> None
   
   let allComms = RouterWide.getAllCommunities config
   let allComms = Seq.choose aux allComms
   if Seq.isEmpty allComms then 0
   else Seq.max allComms

let addOriginComms commMap (config : T) = 
   let max = maxComm config
   let fresh = ref max
   Update.updateActions (fun ((pred, acts) as arg) -> 
      match pred with
      | Comm _ -> Some arg
      | Pred p -> 
         if Route.isTemplate p then 
            let (Filters(oes, fs)) = acts
            
            let oes' = 
               match oes with
               | None -> None
               | Some es -> 
                  incr fresh
                  let c = string !fresh
                  let sc = SetComm(string !fresh)
                  commMap := Map.add p c !commMap
                  Some(List.map (fun (x, ys) -> (x, sc :: ys)) es)
            Some(pred, Filters(oes', fs))
         else Some arg) config

let replaceTemplatePrefixWithComm commMap (config : T) = 
   Update.updateActions (fun ((pred, acts) as arg) -> 
      match pred with
      | Comm _ -> Some arg
      | Pred p -> 
         if Route.isTemplate p then 
            match Map.tryFind p !commMap with
            | None -> Some arg
            | Some c -> Some(Comm(p, c), acts)
         else Some arg) config

let tagAbstractPrefixWithCommunity (config : T) = 
   let commMap = ref Map.empty
   config
   |> addOriginComms commMap
   |> replaceTemplatePrefixWithComm commMap

let moveOriginationToTop (config : T) : T = 
   let aux router rc = 
      let origins, others = 
         List.partition (fun (_, act) -> 
            match act with
            | Filters(Some _, _) -> true
            | Filters _ -> false) rc.Actions
      { rc with Actions = origins @ others }
   
   let rcs = Map.map aux config.RConfigs
   { config with RConfigs = rcs }

let compileAllPrefixes (polInfo : Ast.PolInfo) : CompilationResult = 
   let settings = Args.getSettings()
   
   let mapi = 
      if settings.Parallel then Array.Parallel.mapi
      else Array.mapi
   
   let info = splitConstraints polInfo
   let (aggInfo, _, _) = info
   let pairs = Array.ofList polInfo.Policy
   let timedConfigs, prefixTime = 
      Profile.time 
         (mapi (fun i x -> Profile.time (compileForSinglePrefix (i + 1) polInfo aggInfo) x)) pairs
   let nAggFails = Array.map (fun (res, _) -> res.K) timedConfigs
   let k = Array.fold minFails None nAggFails
   let configs, times = Array.unzip timedConfigs
   let joined, joinTime = Profile.time (joinConfigs polInfo info) (Array.toList configs)
   let joined = moveOriginationToTop joined
   
   let minJoined, minTime = 
      if settings.Minimize then Profile.time (RouterWide.minimize joined) polInfo.Ast.TopoInfo
      else joined, int64 0
   
   let minJoined = 
      minJoined
      |> reindexCommunities
      |> tagAbstractPrefixWithCommunity
   
   let buildTimes = Array.map (fun c -> c.BuildTime) configs
   let minTimes = Array.map (fun c -> c.MinimizeTime) configs
   let aggAnalysisTimes = Array.map (fun c -> c.AggAnalysisTime) configs
   let orderTimes = Array.map (fun c -> c.OrderingTime) configs
   let inboundTimes = Array.map (fun c -> c.InboundAnalysisTime) configs
   let genTimes = Array.map (fun c -> c.ConfigTime) configs
   
   let stats = 
      { NumPrefixes = Array.length configs
        ConfigSize = size minJoined
        PrefixTime = prefixTime
        PerPrefixTimes = times
        PerPrefixBuildTimes = buildTimes
        PerPrefixMinTimes = minTimes
        PerPrefixAggAnalysisTimes = aggAnalysisTimes
        PerPrefixOrderTimes = orderTimes
        PerPrefixInboundTimes = inboundTimes
        PerPrefixGenTimes = genTimes
        JoinTime = joinTime
        MinTime = minTime }
   { Abgp = minJoined
     AggSafety = k
     Stats = stats }

/// Conversion from Abstract BGP to a more low-level format
/// specified in Config.fs. The low-level format includes features 
/// such as prefix lists, community lists, as-path lists, route-maps and so on.
/// The conversion generates incoming and outgoing filters for neighbors 
/// on a per-interface basis.
open Config

let createPolicyList (id, policyLists : List<_>, pls, als, cls) = 
   let name = sprintf "pol-%d" !id
   incr id
   let pol = PolicyList(name, pls, als, cls)
   policyLists.Add(pol)
   pol

let createRouteMap (name, priority, routeMaps : List<_>, rms : List<_>, pol : string, slp, smed, 
                    spre, sc, dc) = 
   let name = sprintf "rm-%s" name
   let rm = RouteMap(name, priority, pol, slp, smed, spre, sc, dc)
   rms.Add(name)
   routeMaps.Add(rm)
   rm

let createPrefixList (kind, id, prefixMap : Dictionary<_, _>, prefixLists : List<_>, pls : List<_>, 
                      prefix) = 
   let b, name = prefixMap.TryGetValue((kind, prefix))
   if b then pls.Add(name)
   else 
      let name = sprintf "pl-%d" !id
      let pl = PrefixList(kind, name, prefix)
      incr id
      pls.Add(name)
      prefixLists.Add(pl)
      prefixMap.[(kind, prefix)] <- name

let createCommunityList (kind, title : Option<string>, communityMap : Dictionary<_, _>, 
                         communityLists : List<_>, cls : List<_>, id, values) = 
   let vs = List.ofSeq values
   match title with
   | None -> 
      let b, name = communityMap.TryGetValue((kind, vs))
      if b then cls.Add(name)
      else 
         let name = sprintf "cl-%d" !id
         let cl = CommunityList(kind, name, values)
         incr id
         cls.Add(name)
         communityLists.Add(cl)
         communityMap.[(kind, vs)] <- name
   | Some name -> 
      let cl = CommunityList(kind, name, values)
      incr id
      cls.Add(name)
      communityLists.Add(cl)
      communityMap.[(kind, vs)] <- name

let createAsPathList (asPathMap : Dictionary<_, _>, asPathLists : List<_>, als : List<_>, id, 
                      reAllow : string list, reDeny : string list) = 
   let b, name = asPathMap.TryGetValue((reAllow, reDeny))
   if b then als.Add(name)
   else 
      let name = sprintf "path-%d" !id
      let ars = List(reAllow)
      let drs = List(reDeny)
      let al = AsPathList(name, ars, drs)
      incr id
      als.Add(name)
      asPathLists.Add(al)
      asPathMap.[(reAllow, reDeny)] <- name

let peers (ti : Topology.TopoInfo) (router : string) = 
   let loc (x : Topology.Node) = x.Loc
   match Topology.findByLoc ti.SelectGraphInfo.Graph router with
   | None -> failwith "unreachable"
   | Some s -> 
      let peers = Topology.neighbors ti.SelectGraphInfo.Graph s
      let inPeers, outPeers = Set.ofSeq peers |> Set.partition Topology.isInside
      let inPeers, outPeers = Set.map loc inPeers, Set.map loc outPeers
      Set.union inPeers outPeers, inPeers, outPeers

let getRouter ti x = 
   let settings = Args.getSettings()
   if settings.IsAbstract then "$" + (Topology.router x ti) + "$"
   else x

let peerPol ti asPathMap asPathLists (als : List<_>) alID (p : Peer) = 
   match p with
   | Peer.Any -> ()
   | Peer.In -> als.Add("path-1")
   | Peer.Out -> als.Add("path-2")
   | Peer.Router peer -> 
      let regexMatch = sprintf "^\(?%s_" (getRouter ti peer)
      createAsPathList (asPathMap, asPathLists, als, alID, [ regexMatch ], [])

let matchAllPeers ti (peers : Set<string>) = 
   let str = 
      peers
      |> Set.map (getRouter ti)
      |> Util.Set.joinBy "|"
   if peers.Count > 1 then sprintf "^\(?(%s)_" str
   else sprintf "^\(?%s_" str

let makeInitialPathList ti peers (asMap, asLists, alID) = 
   if not (Set.isEmpty peers) then 
      let str = matchAllPeers ti peers
      createAsPathList (asMap, asLists, List(), alID, [ str ], []) |> ignore

let getExportComm (p : Peer) maxComm commExportMap = 
   match p with
   | Peer.Any -> maxComm + 2
   | Peer.In -> maxComm + 1
   | Peer.Out -> maxComm
   | Peer.Router x -> Map.find x commExportMap

let peerApplies (p : Peer) (peer : string) (allPeers, inPeers, outPeers) = 
   match p with
   | Peer.In -> Set.contains peer inPeers
   | Peer.Out -> Set.contains peer outPeers
   | Peer.Any -> Set.contains peer allPeers
   | Peer.Router x -> peer = x

let relevantCommsByPeer (exportMap : Reindexer<_>) (all, ins, outs) = 
   let mutable relevantCommMap = Map.empty
   for peer in all do
      exportMap.Iter(fun (p, ms) c -> 
         if peerApplies p peer (all, ins, outs) then 
            let existing = Util.Map.getOrDefault peer Set.empty relevantCommMap
            relevantCommMap <- Map.add peer (Set.add (c, ms) existing) relevantCommMap)
   relevantCommMap

let commGroupsByRelevantPeers m = 
   Map.fold (fun acc k v -> 
      match Map.tryFind v acc with
      | None -> Map.add v (Set.singleton k) acc
      | Some ps -> Map.add v (Set.add k ps) acc) Map.empty m

let getCommunity c = 
   if c = "no-export" then c
   else "200:" + c

let createExportRouteMap pfxInfo (origins : List<Route.TempPrefix * Export list>) peerExportMap id 
    (cMap, cLists, clID) (polLists, polID) rMaps allLocal cs ps = 
   incr id
   let (plID, pfxMap, pLists) = pfxInfo
   let rmname = "export-" + (string !id)
   let mutable priority = 10
   // allow the originated prefix to be exported
   for (pfx, es) in origins do
      let pfx = string pfx
      let pls = List()
      createPrefixList (Config.Kind.Permit, plID, pfxMap, pLists, pls, pfx)
      let pol = createPolicyList (polID, polLists, pls, List(), List())
      let scs = List()
      let mutable smed = null
      let mutable spre = null
      for (peer, ms) in es do
         for m in ms do
            match m with
            | SetComm(c) -> scs.Add(SetCommunity(getCommunity c))
            | SetMed i -> smed <- SetMED(i)
            | PrependPath i -> spre <- SetPathPrepend(i)
      createRouteMap (rmname, priority, rMaps, List(), pol.Name, null, smed, spre, scs, List()) 
      |> ignore
      priority <- priority + 10
   // Create community lists for exports, and collect communities to delete
   // Note: do this even when no export actions
   // since the act of matching some communities will
   // implicitly deny others from being exported
   for (i, ms) in cs do
      let cls = List()
      let dcs = List()
      match i with
      | Some i -> 
         let values = List()
         let v = "100:" + string i
         values.Add(v)
         allLocal := Set.add v !allLocal
         createCommunityList (Config.Kind.Permit, None, cMap, cLists, cls, clID, values)
      //dcs.Add(DeleteCommunity("cl-" + string (!clID - 1)))
      | None -> ()
      let pol = createPolicyList (polID, polLists, List(), List(), cls)
      let scs = List()
      dcs.Add(DeleteCommunity(LOCAL_DELETE_COMMUNITY))
      let mutable smed = null
      let mutable spre = null
      for m in ms do
         match m with
         | SetComm(c) -> scs.Add(SetCommunity(getCommunity c))
         | SetMed i -> smed <- SetMED(i)
         | PrependPath i -> spre <- SetPathPrepend(i)
      createRouteMap (rmname, priority, rMaps, List(), pol.Name, null, smed, spre, scs, dcs) 
      |> ignore
      priority <- priority + 10
   for peer in ps do
      peerExportMap := Map.add peer ("rm-" + rmname) !peerExportMap

let addExportLists pfxInfo origins peerExportMap peerGroups commInfo polInfo rMaps allLocal = 
   let id = ref 0
   peerGroups 
   |> Map.iter 
         (createExportRouteMap pfxInfo origins peerExportMap id commInfo polInfo rMaps allLocal)

let adjustIfTagNotNeeded (m : Map<Set<_>, Set<_>>) = 
   let isUnique = (Map.toSeq m |> Seq.length) = 1
   
   // use none if no tag to be attached
   let inline updateTag a = 
      if isUnique then Some a // None
      else Some a
   
   // change to none if tag doesn't add any information
   let aux acc k v = Map.add (Set.map (fun (a, b) -> ((updateTag a), b)) k) v acc
   Map.fold aux Map.empty m

let computeExportFilters (exportMap : Reindexer<_>) pfxInfo origins peerInfo commInfo polInfo rMaps 
    allLocal = 
   // group export peer by applicable communities
   let relevantCommMap = relevantCommsByPeer exportMap peerInfo
   // group sets of communities with sets of peers
   let peerGroups = commGroupsByRelevantPeers relevantCommMap
   // if only 1 unique action, then no need for tagging
   let peerGroups = adjustIfTagNotNeeded peerGroups
   // add new export community lists / route maps 
   let peerExportMap = ref Map.empty
   addExportLists pfxInfo origins peerExportMap peerGroups commInfo polInfo rMaps allLocal
   !peerExportMap

let importFilterCommunityMods (exportMap : Reindexer<_>) es = 
   let scs = List()
   for (peer, mods) in es do
      let comm = exportMap.Index(peer, Set.ofList mods)
      let c = SetCommunity("100:" + string comm)
      scs.Add(c)
   scs

let toConfig (abgp : T) = 
   let settings = Args.getSettings()
   // policy information
   let pi = abgp.PolInfo
   let ti = pi.Ast.TopoInfo
   // network configuration
   let networkConfig = Dictionary()
   let mutable rid = 0
   for kv in abgp.RConfigs do
      let rname = kv.Key
      let rconfig = kv.Value
      // origin information
      let origins = List()
      let originPfxs = List()
      // peer information
      let (allPeers, inPeers, outPeers) = peers ti rname
      // peer map for peer configurations
      let peerMap = Dictionary()
      // map from unique (peer,mods) pair to community value to attach
      let exportMap = Reindexer(HashIdentity.Structural)
      // low-level filter list ids
      let priority = ref 0
      let (plID, alID, clID, polID) = ref 1, ref 1, ref 1, ref 1
      // all filter lists used
      let (rMaps, pfxLists, asLists, cLists, polLists) = List(), List(), List(), List(), List()
      // map lists to existing names to avoid duplicates
      let (pfxMap, asMap, cMap) = Dictionary(), Dictionary(), Dictionary()
      // create the internal and external as-path lists
      makeInitialPathList ti inPeers (asMap, asLists, alID)
      makeInitialPathList ti outPeers (asMap, asLists, alID)
      // create a route map for each predicate
      for (pred, acts) in rconfig.Actions do
         let tcs = 
            match pred with
            | Pred p -> 
               let x = Route.trafficClassifiers p
               List.map (fun (Route.TrafficClassifier(p, cs)) -> (p, None, cs)) x
            | Comm(p, c) -> 
               let x = Route.trafficClassifiers p
               List.map 
                  (fun (Route.TrafficClassifier(p, cs)) -> 
                  (Route.anyPrefix, Some p, Set.singleton c)) x
         for (prefix, oprefix, comms) in tcs do
            match acts with
            | Filters(o, fs) -> 
               // different actions for the same prefix but different community/regex etc
               match o with
               | None -> ()
               | Some es -> 
                  let prefix = 
                     match oprefix with
                     | None -> prefix
                     | Some p -> p
                  
                  let mostGeneral = prefix.Example()
                  origins.Add((mostGeneral, es))
                  originPfxs.Add(mostGeneral)
               for f in fs do
                  priority := !priority + 10
                  let (rms, pls, als, cls) = List(), List(), List(), List()
                  match f with
                  | Deny -> 
                     // block routes with import filter
                     createPrefixList (Config.Kind.Deny, plID, pfxMap, pfxLists, pls, string prefix) 
                     |> ignore
                     let pol = createPolicyList (polID, polLists, pls, als, cls)
                     createRouteMap 
                        ("in", !priority, rMaps, rms, pol.Name, null, null, null, List(), List()) 
                     |> ignore
                  | Allow((m, lp), es) -> 
                     // must create symmetric export route-map
                     createPrefixList 
                        (Config.Kind.Permit, plID, pfxMap, pfxLists, pls, string prefix) |> ignore
                     // matching based on community
                     let allComms = 
                        match m with
                        | Match.State(c, _) -> Set.add c comms
                        | _ -> comms
                     if allComms.Count > 0 then 
                        for c in allComms do
                           let values = List()
                           values.Add(getCommunity c)
                           createCommunityList 
                              (Config.Kind.Permit, None, cMap, cLists, cls, clID, values)
                     // match based on peer/regex
                     match m with
                     | Match.Peer(x) -> peerPol ti asMap asLists als alID x
                     | Match.State(c, x) -> peerPol ti asMap asLists als alID x
                     | Match.PathRE(re) -> 
                        let (deny, allow) = Regex.split re
                        let allow = Regex.toBgpRegexp allow
                        let deny = Regex.toBgpRegexp deny
                        createAsPathList (asMap, asLists, als, alID, allow, deny) |> ignore
                     let slp = 
                        if lp = 100 then null
                        else SetLocalPref(lp)
                     
                     // add communities to later add modifications for outgoing peers
                     let scs = importFilterCommunityMods exportMap es
                     let pol = createPolicyList (polID, polLists, pls, als, cls)
                     createRouteMap 
                        ("in", !priority, rMaps, rms, pol.Name, slp, null, null, scs, List()) 
                     |> ignore
      // get export filter information
      let allLocalComms = ref Set.empty
      let peerInfo = (allPeers, inPeers, outPeers)
      let peerExportMap = 
         computeExportFilters exportMap (plID, pfxMap, pfxLists) origins peerInfo 
            (cMap, cLists, clID) (polLists, polID) rMaps allLocalComms
      // create a unique community list to delete all local communities
      let allLocalComms = List(Set.toSeq !allLocalComms)
      createCommunityList 
         (Config.Kind.Permit, Some LOCAL_DELETE_COMMUNITY, cMap, cLists, List(), clID, allLocalComms)
      // add import filter for all peers
      for peer in allPeers do
         let export = Map.tryFind peer peerExportMap
         
         let routerIp, peerIp = 
            if settings.IsAbstract || settings.IsTemplate then ("$routerIp$", "peerIp")
            else ti.SelectGraphInfo.IpMap.[(rname, peer)]
         
         let peerName = Topology.router peer ti
         let peerAsn = string ti.SelectGraphInfo.AsnMap.[peerName]
         peerMap.[peerName] <- PeerConfig(peerName, peerAsn, routerIp, peerIp, Some "rm-in", export)
      // create the complete configuration for this router
      let name = Topology.router rname ti
      
      let asn = 
         if settings.IsAbstract then name
         else rname
      
      // get aggregates
      let aggs = List()
      let aggregates = 
         rconfig.Control.Aggregates |> Set.map (fun (pfx, _) -> string <| pfx.Example())
      for a in aggregates do
         aggs.Add(a)
      // unique router id
      rid <- rid + 1
      let routerConfig = 
         RouterConfiguration
            (name, string ti.NetworkAsn, asn, rid, originPfxs, aggs, pfxLists, asLists, cLists, 
             polLists, rMaps, List(peerMap.Values))
      networkConfig.[name] <- routerConfig
   let config = Config.NetworkConfiguration(networkConfig, string ti.NetworkAsn)
   Config.clean config
   config

/// Unit tests for compilation.
/// Hooks into the compileToIR function to test
/// compilation at the per-prefix level.
/// Checks valid import/export filters + preferences
module Test = 
   let isPeer (ain, aout) x m = 
      let check y = 
         match y with
         | Any -> true
         | In -> Set.contains x ain
         | Router y -> x = y
         | Out -> Set.contains x aout
      match m with
      | Peer y -> check y
      | State(_, y) -> check y
      | PathRE r -> string r = "[" + x + "]"
   
   let getPref (ain, aout) (x : string) dc = 
      match dc with
      | Filters(_, fs) -> 
         let lp = 
            List.tryFind (fun f -> 
               match f with
               | Deny -> false
               | Allow((m, _), _) -> isPeer (ain, aout) x m) fs
         match lp with
         | Some(Allow((_, lp), _)) -> lp
         | _ -> 100
   
   let prefersPeer (ain, aout) (_, config) x (a, b) = 
      try 
         let deviceConfig = Map.find x config
         let lp1 = getPref (ain, aout) a deviceConfig
         let lp2 = getPref (ain, aout) b deviceConfig
         lp1 > lp2
      with _ -> false
   
   let receiveFrom (ain, aout) (_, config) x y = 
      let actions = Map.find x config
      match actions with
      | Filters(_, fs) -> 
         List.exists (fun f -> 
            match f with
            | Deny -> false
            | Allow((m, _), _) -> isPeer (ain, aout) y m) fs
   
   let originates (_, config) x = 
      let actions = Map.find x config
      match actions with
      | Filters(Some _, _) -> true
      | Filters _ -> false
   
   type FailReason = 
      | FRInconsistentPrefs
      | FRCantControlPeers
   
   type Test = 
      { Name : string
        Explanation : string
        Topo : Topology.T
        Rf : Route.PredicateBuilder -> Regex.REBuilder -> Regex.T list
        Receive : (string * string) list option
        Originate : string list option
        Prefs : (string * string * string) list option
        Fail : FailReason option }
   
   let tDiamond = Topology.Examples.topoDiamond()
   let tDatacenterSmall = Topology.Examples.topoDatacenterSmall()
   let tDatacenterMedium = Topology.Examples.topoDatacenterMedium()
   let tDatacenterLarge = Topology.Examples.topoDatacenterLarge()
   let tBrokenTriangle = Topology.Examples.topoBrokenTriangle()
   let tBigDipper = Topology.Examples.topoBigDipper()
   let tBadGadget = Topology.Examples.topoBadGadget()
   let tSeesaw = Topology.Examples.topoSeesaw()
   let tStretchingManWAN = Topology.Examples.topoStretchingManWAN()
   let tStretchingManWAN2 = Topology.Examples.topoStretchingManWAN2()
   let tPinCushionWAN = Topology.Examples.topoPinCushionWAN()
   let tBackboneWAN = Topology.Examples.topoBackboneWAN()
   
   let rDiamond1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.Concat(List.map reb.Loc [ "A"; "X"; "N"; "Y"; "B" ])
      [ reb.Build Route.top 1 pref1 ]
   
   let rDiamond2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.Concat(List.map reb.Loc [ "A"; "X"; "N"; "Y"; "B" ])
      
      let pref2 = 
         reb.Concat [ reb.Loc "A"
                      reb.Star reb.Inside
                      reb.Loc "N"
                      reb.Loc "Z"
                      reb.Loc "B" ]
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rDatacenterSmall1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.Internal()
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterSmall2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ]
                     reb.Start [ "B"; "C"; "D" ] ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterSmall3 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let start = reb.Start [ "A"; "B"; "C"; "D" ]
      
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ]
                     start ]
      
      let pref2 = 
         reb.Inter [ reb.Internal()
                     reb.End [ "A" ]
                     start ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rDatacenterSmall4 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.End([ "A" ])
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterSmall5 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let start = reb.Start [ "A"; "B"; "C"; "D" ]
      
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ]
                     start ]
      
      let pref2 = 
         reb.Inter [ reb.Through [ "N" ]
                     reb.End [ "A" ]
                     start ]
      
      let pref3 = 
         reb.Inter [ reb.End [ "A" ]
                     start ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2
        reb.Build Route.top 3 pref3 ]
   
   let rDatacenterMedium1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.Internal()
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterMedium2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Start [ "A" ]
                     reb.Through [ "X" ]
                     reb.End [ "F" ] ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterMedium3 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let vf = 
         reb.ValleyFree([ [ "A"; "B"; "E"; "F" ]
                          [ "C"; "D"; "G"; "H" ]
                          [ "X"; "Y" ] ])
      
      let pref1 = 
         reb.Inter [ reb.Through [ "X" ]
                     reb.End [ "F" ]
                     vf
                     reb.Start [ "A"; "B"; "E"; "F" ] ]
      
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterMedium4 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let vf = 
         reb.ValleyFree([ [ "A"; "B"; "E"; "F" ]
                          [ "C"; "D"; "G"; "H" ]
                          [ "X"; "Y" ] ])
      
      let start = reb.Start([ "A"; "B" ])
      
      let pref1 = 
         reb.Inter [ start
                     reb.Through [ "X" ]
                     reb.End [ "F" ]
                     vf ]
      
      let pref2 = 
         reb.Inter [ reb.End [ "F" ]
                     vf ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rDatacenterMedium5 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let vf = 
         reb.ValleyFree([ [ "A"; "B"; "E"; "F" ]
                          [ "C"; "D"; "G"; "H" ]
                          [ "X"; "Y" ] ])
      
      let pref1 = 
         reb.Inter [ reb.Through [ "X" ]
                     reb.End [ "F" ]
                     vf ]
      
      let pref2 = 
         reb.Inter [ reb.Through [ "Y" ]
                     reb.End [ "F" ]
                     vf ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rDatacenterMedium6 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let vf = 
         reb.ValleyFree([ [ "A"; "B"; "E"; "F" ]
                          [ "C"; "D"; "G"; "H" ]
                          [ "X"; "Y" ] ])
      
      let pref1 = 
         reb.Inter [ reb.Through [ "X" ]
                     reb.End [ "F" ]
                     vf ]
      
      let pref2 = 
         reb.Inter [ reb.Through [ "Y" ]
                     reb.End [ "F" ]
                     vf ]
      
      let pref3 = 
         reb.Inter [ reb.End [ "F" ]
                     vf ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2
        reb.Build Route.top 3 pref3 ]
   
   let rDatacenterLarge1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ] ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rDatacenterLarge2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ] ]
      
      let pref2 = reb.End [ "A" ]
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rDatacenterLarge3 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Through [ "M" ]
                     reb.End [ "A" ] ]
      
      let pref2 = 
         reb.Inter [ reb.Through [ "N" ]
                     reb.End [ "A" ] ]
      
      let pref3 = reb.End [ "A" ]
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2
        reb.Build Route.top 3 pref3 ]
   
   let rBrokenTriangle1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Inter [ reb.Union [ reb.Path [ [ "C" ]
                                            [ "A" ]
                                            [ "E" ]
                                            [ "D" ] ]
                                 reb.Path [ [ "A" ]
                                            [ "B" ]
                                            [ "D" ] ] ]
                     reb.Start [ "A"; "C"; "D" ] ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rBigDipper1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let op1 = 
         reb.Path [ [ "C" ]
                    [ "A" ]
                    [ "E" ]
                    [ "D" ] ]
      
      let op2 = 
         reb.Path [ [ "A" ]
                    [ "E" ]
                    [ "D" ] ]
      
      let op3 = 
         reb.Path [ [ "A" ]
                    [ "D" ] ]
      
      let pref1 = reb.Union [ op1; op2; op3 ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rBadGadget1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let op1 = 
         reb.Path [ [ "A" ]
                    [ "C" ]
                    [ "D" ] ]
      
      let op2 = 
         reb.Path [ [ "B" ]
                    [ "A" ]
                    [ "D" ] ]
      
      let op3 = 
         reb.Path [ [ "C" ]
                    [ "B" ]
                    [ "D" ] ]
      
      let pref1 = reb.Union [ op1; op2; op3 ]
      
      let op4 = 
         reb.Path [ [ "A" ]
                    [ "D" ] ]
      
      let op5 = 
         reb.Path [ [ "B" ]
                    [ "D" ] ]
      
      let op6 = 
         reb.Path [ [ "C" ]
                    [ "D" ] ]
      
      let pref2 = reb.Union [ op4; op5; op6 ]
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rBadGadget2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let op1 = 
         reb.Path [ [ "A" ]
                    [ "C" ]
                    [ "D" ] ]
      
      let op2 = 
         reb.Path [ [ "B" ]
                    [ "A" ]
                    [ "D" ] ]
      
      let op3 = 
         reb.Path [ [ "C" ]
                    [ "B" ]
                    [ "D" ] ]
      
      let op4 = 
         reb.Path [ [ "A" ]
                    [ "D" ] ]
      
      let op5 = 
         reb.Path [ [ "B" ]
                    [ "D" ] ]
      
      let op6 = 
         reb.Path [ [ "C" ]
                    [ "D" ] ]
      
      let pref1 = reb.Union [ op1; op2; op3; op4; op5; op6 ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rSeesaw1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let op1 = 
         reb.Path [ [ "A" ]
                    [ "X" ]
                    [ "N" ]
                    [ "M" ] ]
      
      let op2 = 
         reb.Path [ [ "B" ]
                    [ "X" ]
                    [ "N" ]
                    [ "M" ] ]
      
      let op3 = 
         reb.Path [ [ "A" ]
                    [ "X" ]
                    [ "O" ]
                    [ "M" ] ]
      
      let op4 = 
         reb.Path [ [ "X" ]
                    [ "O" ]
                    [ "M" ] ]
      
      let pref1 = reb.Union [ op1; op2; op3; op4 ]
      
      let pref2 = 
         reb.Path [ [ "X" ]
                    [ "N" ]
                    [ "M" ] ]
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rStretchingManWAN1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Concat [ reb.Star reb.Outside
                      reb.Loc "A"
                      reb.Star reb.Inside
                      reb.Loc "Y" ]
      
      let pref2 = 
         reb.Concat [ reb.Star reb.Outside
                      reb.Loc "B"
                      reb.Star reb.Inside
                      reb.Outside ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rStretchingManWAN2 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Concat [ reb.Outside
                      reb.Loc "A"
                      reb.Star reb.Inside
                      reb.Loc "Y" ]
      
      let pref2 = 
         reb.Concat [ reb.Outside
                      reb.Loc "B"
                      reb.Star reb.Inside
                      reb.Outside ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rStretchingManWAN3 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Concat [ reb.Star reb.Outside
                      reb.Loc "A"
                      reb.Star reb.Inside
                      reb.Loc "Y"
                      reb.Star reb.Outside
                      reb.Loc "ASChina" ]
      [ reb.Build Route.top 1 pref1 ]
   
   let rStretchingManWAN4 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Concat [ reb.Loc "W"
                      reb.Loc "A"
                      reb.Loc "C"
                      reb.Loc "D"
                      reb.Outside ]
      
      let pref2 = 
         reb.Concat [ reb.Loc "W"
                      reb.Loc "B"
                      reb.Internal()
                      reb.Outside ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rPinCushionWAN1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = 
         reb.Concat [ reb.Loc "W"
                      reb.Internal()
                      reb.Loc "Y" ]
      
      let pref2 = 
         reb.Concat [ reb.Loc "X"
                      reb.Internal()
                      reb.Loc "Z" ]
      
      [ reb.Build Route.top 1 pref1
        reb.Build Route.top 2 pref2 ]
   
   let rBackboneWAN1 (pb : Route.PredicateBuilder) (reb : Regex.REBuilder) = 
      let pref1 = reb.End([ "A" ])
      [ reb.Build Route.top 1 pref1 ]
   
   let tests (settings : Args.T) = 
      let controlIn = settings.UseMed || settings.UsePrepending
      let noExport = settings.UseNoExport
      [ { Name = "Diamond1"
          Explanation = "A simple path"
          Topo = tDiamond
          Rf = rDiamond1
          Receive = 
             Some [ ("Y", "B")
                    ("N", "Y")
                    ("X", "N")
                    ("A", "X") ]
          Originate = Some [ "B" ]
          Prefs = Some []
          Fail = None }
        { Name = "Diamond2"
          Explanation = "Impossible Backup (should fail)"
          Topo = tDiamond
          Rf = rDiamond2
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DCsmall1"
          Explanation = "Shortest paths routing"
          Topo = tDatacenterSmall
          Rf = rDatacenterSmall1
          Receive = Some []
          Originate = Some [ "A"; "B"; "C"; "D" ]
          Prefs = Some []
          Fail = None }
        { Name = "DCsmall2"
          Explanation = "Through spine no backup"
          Topo = tDatacenterSmall
          Rf = rDatacenterSmall2
          Receive = Some []
          Originate = Some [ "A" ]
          Prefs = Some []
          Fail = None }
        { Name = "DCsmall3"
          Explanation = "Through spine with backup"
          Topo = tDatacenterSmall
          Rf = rDatacenterSmall3
          Receive = 
             Some [ ("X", "A")
                    ("M", "X")
                    ("N", "X")
                    ("B", "X")
                    ("Y", "M")
                    ("Y", "N")
                    ("C", "Y")
                    ("D", "Y") ]
          Originate = Some [ "A" ]
          Prefs = Some [ ("Y", "M", "N") ]
          Fail = None }
        { Name = "DCsmall4"
          Explanation = "End at single location"
          Topo = tDatacenterSmall
          Rf = rDatacenterSmall4
          Receive = 
             Some [ ("X", "A")
                    ("M", "X")
                    ("N", "X")
                    ("Y", "M")
                    ("Y", "N")
                    ("C", "Y")
                    ("D", "Y") ]
          Originate = Some [ "A" ]
          Prefs = Some []
          Fail = None }
        { Name = "DCsmall5"
          Explanation = "Through spine to single location (should fail)"
          Topo = tDatacenterSmall
          Rf = rDatacenterSmall5
          Receive = 
             Some [ ("X", "A")
                    ("M", "X")
                    ("N", "X")
                    ("Y", "M")
                    ("Y", "N")
                    ("C", "Y")
                    ("D", "Y")
                    ("C", "Y") ]
          Originate = Some [ "A" ]
          Prefs = Some [ ("Y", "M", "N") ]
          Fail = None }
        { Name = "DCmedium1"
          Explanation = "Shortest paths routing"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium1
          Receive = Some []
          Originate = Some []
          Prefs = Some []
          Fail = None }
        { Name = "DCmedium2"
          Explanation = "Through spine (should fail)"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium2
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DCmedium3"
          Explanation = "Through spine, valley free (should fail)"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium3
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DCmedium4"
          Explanation = "Through spine, valley free with simple backup"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium4
          Receive = 
             Some [ ("G", "F")
                    ("H", "F")
                    ("E", "G")
                    ("E", "H")
                    ("X", "G")
                    ("X", "H")
                    ("Y", "G")
                    ("Y", "H")
                    ("C", "X")
                    ("C", "Y")
                    ("D", "X")
                    ("D", "Y")
                    ("A", "C")
                    ("A", "D")
                    ("B", "C")
                    ("B", "D")
                    ("H", "X")
                    ("H", "Y")
                    ("G", "X")
                    ("G", "Y") ]
          (* Strange, but safe *)
          Originate = Some [ "F" ]
          Prefs = 
             Some [ ("C", "X", "Y")
                    ("D", "X", "Y")
                    ("G", "F", "X")
                    ("G", "F", "Y")
                    ("H", "F", "X")
                    ("H", "F", "Y") ]
          Fail = None }
        { Name = "DCmedium5"
          Explanation = "Spine Preferences, no backup (should fail)"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium5
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DCmedium6"
          Explanation = "Spine Preferences with backup (should fail)"
          Topo = tDatacenterMedium
          Rf = rDatacenterMedium6
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DClarge1"
          Explanation = "Through spine (should fail)"
          Topo = tDatacenterLarge
          Rf = rDatacenterLarge1
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DClarge2"
          Explanation = "Through spine with backup (should fail)"
          Topo = tDatacenterLarge
          Rf = rDatacenterLarge2
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "DClarge3"
          Explanation = "Through spines w/prefs + backup (should fail)"
          Topo = tDatacenterLarge
          Rf = rDatacenterLarge3
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "BrokenTriangle"
          Explanation = "Inconsistent path suffixes (should fail)"
          Topo = tBrokenTriangle
          Rf = rBrokenTriangle1
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "BigDipper"
          Explanation = "Must choose the correct preference"
          Topo = tBigDipper
          Rf = rBigDipper1
          Receive = 
             Some [ ("E", "D")
                    ("A", "E")
                    ("C", "A") ]
          Originate = Some [ "D" ]
          Prefs = Some [ ("A", "E", "D") ]
          Fail = None }
        { Name = "BadGadget"
          Explanation = "Total ordering prevents instability (should fail)"
          Topo = tBadGadget
          Rf = rBadGadget1
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "OkGadget"
          Explanation = "Must find correct total ordering"
          Topo = tBadGadget
          Rf = rBadGadget2
          Receive = 
             Some [ ("A", "D")
                    ("B", "D")
                    ("C", "D") ]
          Originate = Some [ "D" ]
          Prefs = 
             Some [ ("A", "D", "C")
                    ("B", "D", "A")
                    ("C", "D", "B") ]
          Fail = None }
        { Name = "Seesaw"
          Explanation = "Must get all best preferences (should fail)"
          Topo = tSeesaw
          Rf = rSeesaw1
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        ((* TODO: test preferences on filters *)
         if controlIn then 
            { Name = "StretchingMan1"
              Explanation = "Can't control inbound traffic"
              Topo = tStretchingManWAN
              Rf = rStretchingManWAN1
              Receive = 
                 Some [ ("D", "Y")
                        ("D", "Z")
                        ("C", "D")
                        ("A", "C")
                        ("B", "C") ]
              Originate = Some []
              Prefs = Some []
              Fail = None }
         else 
            { Name = "StretchingMan1"
              Explanation = "Can't control inbound traffic"
              Topo = tStretchingManWAN
              Rf = rStretchingManWAN1
              Receive = None
              Originate = None
              Prefs = None
              Fail = Some FRCantControlPeers })
        (if controlIn && noExport then 
            { Name = "StretchingMan2"
              Explanation = "Prefer one AS over another"
              Topo = tStretchingManWAN
              Rf = rStretchingManWAN2
              Receive = 
                 Some [ ("C", "D")
                        ("A", "C")
                        ("B", "C") ]
              Originate = Some []
              Prefs = Some [ ("D", "Y", "Z") ]
              Fail = None }
         else 
            { Name = "StretchingMan2"
              Explanation = "Prefer one AS over another"
              Topo = tStretchingManWAN
              Rf = rStretchingManWAN2
              Receive = None
              Originate = None
              Prefs = None
              Fail = Some FRCantControlPeers })
        { Name = "StretchingMan3"
          Explanation = "Using peer not listed in the topology"
          Topo = tStretchingManWAN
          Rf = rStretchingManWAN3
          Receive = Some []
          Originate = Some []
          Prefs = Some []
          Fail = None }
        (// TODO: test filters etc
         if controlIn && noExport then 
            { Name = "StretchingMan4"
              Explanation = "Use MED, Prepending, No-export, and Local-pref"
              Topo = tStretchingManWAN2
              Rf = rStretchingManWAN4
              Receive = Some []
              Originate = Some []
              Prefs = Some []
              Fail = None }
         else 
            { Name = "StretchingMan4"
              Explanation = "Use MED, Prepending, No-export, and Local-pref"
              Topo = tStretchingManWAN2
              Rf = rStretchingManWAN4
              Receive = None
              Originate = None
              Prefs = None
              Fail = Some FRCantControlPeers })
        { Name = "PinCushion"
          Explanation = "Unimplementable peer preference"
          Topo = tPinCushionWAN
          Rf = rPinCushionWAN1
          Receive = None
          Originate = None
          Prefs = None
          Fail = Some FRInconsistentPrefs }
        { Name = "Backbone"
          Explanation = "Incoming traffic from multiple peers"
          Topo = tBackboneWAN
          Rf = rBackboneWAN1
          Receive = 
             Some [ ("NY", "A")
                    ("SEA", "A") ]
          Originate = Some [ "A" ]
          Prefs = Some []
          Fail = None } ]
   
   (*  
   let testAggregationFailure() = 
      printf "Aggregation failures "
      let pb = Route.PredicateBuilder()
      let topo = Topology.Examples.topoDatacenterMedium()
      let reb = Regex.REBuilder(topo)
      let pol = reb.End [ "A" ]
      let aggs = 
         Map.add "X" 
            (Set.singleton (Route.Prefix(10, 0, 0, 0, 31, Route.Range(31, 32)), [ "PEER" ])) 
            Map.empty
      let aggs = 
         Map.add "Y" 
            (Set.singleton (Route.Prefix(10, 0, 0, 0, 31, Route.Range(31, 32)), [ "PEER" ])) aggs
      let res = 
         compileToIR 0 (Route.prefix <| Route.Prefix(10, 0, 0, 0, 32)) None aggs reb 
            [ reb.Build Route.top 1 pol ]
      match res with
      | Err _ -> failed()
      | Ok(res) -> 
         match res.K with
         | Some x when x.NumFailures = 1 -> passed()
         | _ -> failed()
   
   let testCompilation() = 
      let border = String.replicate 80 "-"
      printfn "%s" border
      let settings = Args.getSettings()
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
         logInfo (0, "\n" + msg)
         let (ain, aout) = Topology.alphabet test.Topo
         let ain = ain |> Set.map (fun v -> v.Loc)
         let aout = aout |> Set.map (fun v -> v.Loc)
         let reb = Regex.REBuilder(test.Topo)
         let pb = Route.PredicateBuilder()
         let built = test.Rf pb reb
         if not (Topology.isWellFormed test.Topo) then 
            failed()
            logInfo (0, msg)
         else
            let pred = Route.top
            match compileToIR 0 pred None Map.empty reb built with
            | Err(x) -> 
               if (Option.isSome test.Receive || Option.isSome test.Originate 
                   || Option.isSome test.Prefs || Option.isNone test.Fail) then 
                  failed()
                  let msg = sprintf "[Failed]: Name: %s, should compile but did not" test.Name
                  logInfo (0, msg)
               match test.Fail, x with
               | Some FRInconsistentPrefs, InconsistentPrefs _ -> passed()
               | Some FRCantControlPeers, UncontrollableEnter _ -> passed()
               | Some FRCantControlPeers, UncontrollablePeerPreference _ -> passed()
               | _ -> 
                  failed()
                  let msg = sprintf "[Failed]: Name: %s, expected error" test.Name
                  logInfo (0, msg)
            | Ok(res) -> 
               let config = res.Config
               if (Option.isNone test.Receive || Option.isNone test.Originate 
                   || Option.isNone test.Prefs || Option.isSome test.Fail) then 
                  failed()
                  let msg = sprintf "[Failed]: Name: %s, should not compile but did" test.Name
                  logInfo (0, msg)
               else 
                  let mutable fail = false
                  let rs = Option.get test.Receive
                  for (x, y) in rs do
                     if not (receiveFrom (ain, aout) config x y) then 
                        fail <- true
                        let msg = 
                           sprintf "[Failed]: (%s) - %s should receive from %s but did not" 
                              test.Name x y
                        logInfo (0, msg)
                  let os = Option.get test.Originate
                  for x in os do
                     if not (originates config x) then 
                        fail <- true
                        let msg = 
                           sprintf "[Failed]: (%s) - %s should originate a route but did not" 
                              test.Name x
                        logInfo (0, msg)
                  let ps = Option.get test.Prefs
                  for (x, a, b) in ps do
                     if not (prefersPeer (ain, aout) config x (a, b)) then 
                        fail <- true
                        let msg = 
                           sprintf "[Failed]: (%s) - %s should prefer %s to %s but did not" 
                              test.Name x a b
                        logInfo (0, msg)
                  if fail then failed()
                  else passed()
      printfn "%s" border *)
   let run() = ()
// testAggregationFailure()
// testCompilation()