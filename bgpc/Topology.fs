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


module Examples =
    type Tiers = Dictionary<State,int>
    type Prefixes = Dictionary<State,Prefix.T>

    let megaDC (tiers: (int*int) list) top =
        let loc t i = 
            "T" + string t + "_" + string i 
        // generate and store new prefixes as we go
        let currPrefix = ref 0
        let prefixMap = Dictionary()
        // remeber what tier nodes are in
        let tierMap = Dictionary()
        // build a topology from tiers of a datacenter
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let maxTier = List.length tiers
        // recursively construct a data center from the top down
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
                            let a = uint32 (!currPrefix % 256 * 256 * 256)
                            let b = uint32 (!currPrefix % 256 * 256)
                            let c = uint32 (!currPrefix % 256)
                            let p = Prefix.prefix (a, b, c, 0u) 24u
                            prefixMap.[v] <- p
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

    let fatTree (k: int) : T * Prefixes * Tiers = 
        let iT0 = 2*k
        let iT1 = 2*k
        let iT2 = k
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let prefixes = Dictionary()
        let routersT0 = Array.init iT0 (fun i ->
            let name = "T0_" + string i
            let a = uint32 (i % 256 * 256 * 256)
            let b = uint32 (i % 256 * 256)
            let c = uint32 (i % 256)
            let p = Prefix.prefix (a, b, c, 0u) 24u
            prefixes.[name] <- p
            name)
        let routersT1 = Array.init iT1 (fun i -> "T1_" + string i)
        let routesrT2 = Array.init iT2 (fun i -> "T2_" + string i)

        for i in 0 .. iT1 .. 2 do 
            
            ()

        failwith ""