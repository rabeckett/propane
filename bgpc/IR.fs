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
            sb.Append(" with local-pref=" + lp.ToString()) |> ignore
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

(* If we prefer X back to back from two different states, either way we will choose X 
let rec removeAdjacentLocs sorted = 
    match sorted with 
    | [] | [_] -> sorted
    | hd1::((hd2::z) as tl) ->
        let (x,i1) = hd1 
        let (y,i2) = hd2 
        if x.Node.Loc = y.Node.Loc then removeAdjacentLocs (hd1::z)
        else hd1 :: (removeAdjacentLocs tl) *)


(* TODO: compress by topology *)
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
                debug1 (fun () -> System.IO.File.WriteAllText(outName + ".ir", format config))
                Ok (config)
            | Err((x,y)) -> Err(InconsistentPrefs(x,y))
