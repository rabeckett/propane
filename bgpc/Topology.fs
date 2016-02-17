module Topology

open Common.Error
open Common.Color
open QuickGraph
open QuickGraph.Algorithms
open System.Xml
open FSharp.Data
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
            match a,b with 
            | Some a, Some b -> 
                let ns = 
                    topo.OutEdges a
                    |> Seq.map (fun (e: TaggedEdge<State,unit>) -> e.Target)
                    |> Set.ofSeq
                if Set.contains b ns then 
                    pairs <- (a, b) :: pairs
            | _, _ -> ()
    pairs


type Topo = XmlProvider<"../examples/dc.xml">

type TopoInfo =
    {Graph: T; 
     AsnMap: Map<string, uint32>;
     InternalNames: Set<string>;
     ExternalNames: Set<string>;
     AllNames: Set<string>}

let inline getAsn name asn =
    if asn < 0 then 
        error (sprintf "Negate AS number '%d' in topology for node '%s'" asn name)
    else uint32 asn

let readTopology (file: string) : TopoInfo =
    let g = BidirectionalGraph<State,TaggedEdge<State,unit>>()
    let topo = Topo.Load file
    let mutable nodeMap = Map.empty
    let mutable asnMap = Map.empty
    let mutable internalNames = Set.empty
    let mutable externalNames = Set.empty
    for n in topo.Nodes do
        match Map.tryFind n.Name asnMap with
        | None -> asnMap <- Map.add n.Name (getAsn n.Name n.Asn) asnMap
        | Some _ -> error (sprintf "Duplicate router name '%s' in topology" n.Name)
        let typ = 
            match n.Internal, n.CanOriginate with 
            | true, false -> Inside
            | true, true -> InsideOriginates
            | false, true
            | false, false -> Outside
        if n.Internal then 
            internalNames <- Set.add n.Name internalNames
        else 
            externalNames <- Set.add n.Name externalNames
        // TODO: duplicate names not handled
        let asn = string n.Asn
        let state = {Loc = asn; Typ=typ}
        nodeMap <- Map.add n.Name state nodeMap
        ignore (g.AddVertex state)

    for e in topo.Edges do 
        if not (nodeMap.ContainsKey e.Source) then 
            error (sprintf "Invalid edge source location %s in topology" e.Source)
        elif not (nodeMap.ContainsKey e.Target) then 
            error (sprintf "Invalid edge target location %s in topology" e.Target)
        else
            let x = nodeMap.[e.Source]
            let y = nodeMap.[e.Target]
            if e.Directed then
                addEdgesDirected g [(x,y)]
            else 
                addEdgesUndirected g [(x,y)]
    {Graph = g; 
     AsnMap = asnMap; 
     InternalNames = internalNames; 
     ExternalNames = externalNames; 
     AllNames = Set.union internalNames externalNames }
