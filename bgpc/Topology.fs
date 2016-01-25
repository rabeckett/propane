module Topology

open QuickGraph
open QuickGraph.Algorithms
open System.Collections.Generic

type NodeType = 
    | Start
    | End
    | Outside
    | Inside 
    | InsideOriginates
    | Unknown

type State = 
    {Loc: string; 
     Typ: NodeType}

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

exception InvalidTopologyException

let copyTopology (topo: T) : T = 
    let newTopo = BidirectionalGraph<State,TaggedEdge<State,unit>>()
    for v in topo.Vertices do newTopo.AddVertex v |> ignore
    for e in topo.Edges do newTopo.AddEdge e |> ignore 
    newTopo

let alphabet (topo: T) : Set<State> * Set<State> = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.Typ with 
        | Inside | InsideOriginates -> ain <- Set.add v ain
        | Outside | Unknown -> aout <- Set.add v aout
        | Start | End -> failwith "unreachable"
    (ain, aout)

let isTopoNode (t: State) = 
    match t.Typ with 
    | Start | End -> false
    | _ -> true

let isOutside (t: State) = 
    match t.Typ with 
    | Outside -> true
    | Unknown -> true
    | _ -> false

let isInside (t: State) = 
    match t.Typ with 
    | Inside -> true 
    | InsideOriginates ->  true 
    | _ -> false

let canOriginateTraffic (t: State) = 
    match t.Typ with 
    | InsideOriginates -> true 
    | Outside -> true
    | Unknown -> true
    | Inside -> false
    | Start | End -> false

let isPeer (topo: T) (state: State) =
    let receivesFromInside = 
        topo.InEdges state
        |> Seq.map (fun e -> e.Source)
        |> Seq.exists isInside
    (isOutside state) && receivesFromInside

let isWellFormed (topo: T) : bool =
    let onlyInside = copyTopology topo
    onlyInside.RemoveVertexIf (fun v -> isOutside v) |> ignore
    let d = Dictionary<State,int>()
    ignore (onlyInside.WeaklyConnectedComponents d)
    (Set.ofSeq d.Values).Count = 1

let rec addVertices (topo: T) (vs: State list) = 
    match vs with 
    | [] -> ()
    | v::vs -> 
        topo.AddVertex v |> ignore
        addVertices topo vs

let rec addEdgesUndirected (topo: T) (es: (State * State) list) = 
    match es with 
    | [] -> () 
    | (x,y)::es -> 
        topo.AddEdge (TaggedEdge(x,y,())) |> ignore
        topo.AddEdge (TaggedEdge(y,x,())) |> ignore
        addEdgesUndirected topo es

let rec addEdgesDirected (topo: T) (es: (State * State) list) = 
    match es with 
    | [] -> () 
    | (x,y)::es -> 
        topo.AddEdge (TaggedEdge(x,y,())) |> ignore
        addEdgesDirected topo es

let getStateByLoc (topo: T) loc = 
    Seq.tryFind (fun v -> v.Loc = loc) topo.Vertices
    
let findLinks (topo: T) (froms, tos) =
    let mutable pairs = []
    for x in Set.toSeq froms do 
        for y in Set.toSeq tos do 
            let a = getStateByLoc topo x 
            let b = getStateByLoc topo y
            match a, b with
            | Some s, Some d ->
                let ns = 
                    topo.OutEdges s
                    |> Seq.map (fun (e: TaggedEdge<State,unit>) -> e.Target)
                    |> Set.ofSeq
                if Set.contains d ns then 
                    pairs <- (s, d) :: pairs
            | _, _ -> ()
    pairs

let peers (topo:T) = 
    Seq.filter (fun v ->
        isOutside v && 
        topo.OutEdges v 
        |> Seq.map (fun e -> e.Target) 
        |> Seq.exists isInside
    ) topo.Vertices 


module Examples =
    type Tiers = Dictionary<State,int>
    type Prefixes = Dictionary<State,Prefix.T>

    let getPrefix i = 
        let a = uint32 (i / (256 * 256))
        let b = uint32 (i / 256)
        let c = uint32 (i % 256)
        Prefix.prefix (a, b, c, 0u) 24u

    let megaDC (tiers: (int*int) list) top =
        let loc t i = 
            "T" + string t + "_" + string i 
        let currPrefix = ref 0
        let prefixMap = Dictionary()
        let tierMap = Dictionary()
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let maxTier = List.length tiers
        let rec aux currTier i parents (tiers: _ list) =
            match tiers with
            | [] -> ()
            | (routers,blocks)::tl ->
                for b in 0..blocks-1 do
                    let mutable newParents = []
                    for r in 0..routers-1 do
                        let idx = (b*routers + r) + blocks*routers*i
                        let l = loc currTier idx
                        let v = {Loc=l; Typ=if currTier=0 then InsideOriginates else Inside}
                        printfn "Adding vertex: %s" v.Loc
                        g.AddVertex v |> ignore
                        tierMap.[v] <- currTier
                        if currTier = 0 then 
                            prefixMap.[v] <- getPrefix !currPrefix
                            currPrefix := !currPrefix + 1
                        for u in parents do
                            printfn "Adding edge: %s to %s" v.Loc u.Loc
                            g.AddEdge (TaggedEdge(v,u,())) |> ignore
                            g.AddEdge (TaggedEdge(u,v,())) |> ignore
                        newParents <- v :: newParents
                    if currTier = 0 then () 
                    else aux (currTier - 1) b newParents tl
        aux maxTier 0 [] ((top,1) :: List.rev tiers)
        (g, prefixMap, tierMap)

    let fatTree k : T * Prefixes * Tiers = 
        let iT0 = (k * k) / 2
        let iT1 = (k * k) / 2
        let iT2 = (k * k) / 4
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let core = {Loc="CORE"; Typ=Outside}
        let idfx = {Loc="IDFX"; Typ=Outside}
        ignore (g.AddVertex core)
        ignore (g.AddVertex idfx)
        let prefixes = Dictionary()
        let tiers = Dictionary()
        let routersT0 = Array.init iT0 (fun i ->
            let name = "T0_" + string i
            let v = {Loc=name; Typ=InsideOriginates}
            ignore (g.AddVertex v)
            prefixes.[v] <- getPrefix i
            tiers.[v] <- 0
            v)
        let routersT1 = Array.init iT1 (fun i -> 
            let name = "T1_" + string i
            let v = {Loc=name; Typ=Inside}
            ignore (g.AddVertex v)
            tiers.[v] <- 1
            v)
        let routersT2 = Array.init iT2 (fun i ->
            let name = "T2_" + string i
            let v = {Loc=name; Typ=Inside}
            ignore (g.AddVertex v)
            tiers.[v] <- 2
            v)
        let perPod = (k/2)
        for i = 0 to  iT0-1 do
            let pod = i / (perPod)
            for j = 0 to perPod-1 do
                let x = routersT0.[i]
                let y = routersT1.[pod*perPod + j]
                addEdgesUndirected g [(x,y)]
        for i = 0 to iT1-1 do 
            for j = 0 to perPod-1 do
                let rem = i % perPod
                let x = routersT1.[i]
                let y = routersT2.[rem*perPod + j]
                addEdgesUndirected g [(x,y)]
        for i = 0 to iT2-1 do 
            let x = routersT2.[i]
            addEdgesUndirected g [(core, x)]
            addEdgesUndirected g [(idfx, x)]
        (g, prefixes, tiers)

    let complete n  = 
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let numPeers = max 3 (n / 2)
        let getPeer i = 
            let typ = 
                match i % 3 with
                | 0 -> "Cust"
                | 1 -> "Peer"
                | 2 -> "Paid"
                | _ -> failwith "unreachable"
            let which = string (i / 3)
            typ + which
         // setup external peers
        let externalPeers = Dictionary()
        for i = 0 to numPeers-1 do 
            let name = getPeer i
            let v = {Loc=name; Typ=Outside}
            ignore (g.AddVertex v)
            externalPeers.[name] <- v
        // setup internal full mesh
        for i = 0 to n-1 do
            let name = "R" + string i
            let v = {Loc=name; Typ=Inside}
            ignore (g.AddVertex v)
        for v1 in g.Vertices do 
            for v2 in g.Vertices do 
                if isInside v1 && isInside v2 && v1 <> v2 then
                    g.AddEdge (TaggedEdge(v1,v2,())) |> ignore
        // add connections to external peers
        let mutable i = 0 
        for v in g.Vertices do 
            if isInside v then
                let outside = externalPeers.[getPeer i]
                g.AddEdge (TaggedEdge (v, outside, ()) ) |> ignore
                g.AddEdge (TaggedEdge (outside, v, ()) ) |> ignore
                i <- i + 1
        g