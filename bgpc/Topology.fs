module Topology

open QuickGraph

type NodeType = 
    | Start
    | Outside
    | Inside 
    | InsideHostConnected

type State = 
    {Loc: string; 
     Typ: NodeType}

type NeighborMap = Map<State, Set<State>>

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

let alphabet (topo: T) : Set<State> * Set<State> = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.Typ with 
        | Inside -> ain <- Set.add v ain
        | InsideHostConnected -> ain <- Set.add v ain
        | Outside -> aout <- Set.add v aout
        | Start -> failwith "unreachable"
    (ain, aout)

let neighborMap (topo: T) : NeighborMap   = 
    let mutable nmap = Map.empty
    for v in topo.Vertices do
        let mutable adj = Set.empty 
        for e in topo.OutEdges v do 
            adj <- Set.add e.Target adj
        for e in topo.InEdges v do 
            adj <- Set.add e.Source adj
        nmap <- Map.add v adj nmap
    nmap

let isEndHostConnected (t: State) = 
    match t.Typ with 
    | InsideHostConnected -> true 
    | Outside -> true
    | Inside -> false
    | Start -> false

(* TODO: check for duplicate topology nodes *)
(* TODO: well defined in and out (fully connected inside) *)
let isWellFormed (t: State) = 
    failwith "TODO"

module Example1 = 
    let topo () = 
        let g = BidirectionalGraph<State ,TaggedEdge<State,unit>>()
        let vA = {Loc="A"; Typ=InsideHostConnected}
        let vX = {Loc="X"; Typ=Inside}
        let vM = {Loc="M"; Typ=Inside}
        let vN = {Loc="N"; Typ=Inside}
        let vY = {Loc="Y"; Typ=Inside}
        let vZ = {Loc="Z"; Typ=Inside}
        let vB = {Loc="B"; Typ=InsideHostConnected}
        g.AddVertex vA |> ignore 
        g.AddVertex vX |> ignore 
        g.AddVertex vM |> ignore 
        g.AddVertex vN |> ignore 
        g.AddVertex vY |> ignore 
        g.AddVertex vZ |> ignore 
        g.AddVertex vB |> ignore 
        g.AddEdge (TaggedEdge(vA, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vA, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vZ, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vZ, vB, ())) |> ignore
        g

module Example2 = 
    let topo () = 
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let vA = {Loc="A"; Typ=InsideHostConnected}
        let vB = {Loc="B"; Typ=InsideHostConnected}
        let vC = {Loc="C"; Typ=InsideHostConnected}
        let vD = {Loc="D"; Typ=InsideHostConnected}
        let vX = {Loc="X"; Typ=Inside}
        let vY = {Loc="Y"; Typ=Inside}
        let vM = {Loc="M"; Typ=Inside}
        let vN = {Loc="N"; Typ=Inside}
        g.AddVertex vA |> ignore 
        g.AddVertex vB |> ignore 
        g.AddVertex vC |> ignore 
        g.AddVertex vD |> ignore 
        g.AddVertex vX |> ignore 
        g.AddVertex vY |> ignore 
        g.AddVertex vM |> ignore 
        g.AddVertex vN |> ignore 
        g.AddEdge (TaggedEdge(vA, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vA, ())) |> ignore
        g.AddEdge (TaggedEdge(vB, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vD, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vY, ())) |> ignore
        g

module Example3 = 
    let topo () = 
        let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
        let vA = {Loc="A"; Typ=InsideHostConnected}
        let vB = {Loc="B"; Typ=InsideHostConnected}
        let vC = {Loc="C"; Typ=Inside}
        let vD = {Loc="D"; Typ=Inside}
        let vE = {Loc="E"; Typ=InsideHostConnected}
        let vF = {Loc="F"; Typ=InsideHostConnected}
        let vG = {Loc="G"; Typ=Inside}
        let vH = {Loc="H"; Typ=Inside}
        let vX = {Loc="X"; Typ=Inside}
        let vY = {Loc="Y"; Typ=Inside}
        g.AddVertex vA |> ignore 
        g.AddVertex vB |> ignore 
        g.AddVertex vC |> ignore 
        g.AddVertex vD |> ignore 
        g.AddVertex vE |> ignore 
        g.AddVertex vF |> ignore 
        g.AddVertex vG |> ignore 
        g.AddVertex vH |> ignore 
        g.AddVertex vX |> ignore
        g.AddVertex vY |> ignore
        g.AddEdge (TaggedEdge(vA, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vA, ())) |> ignore
        g.AddEdge (TaggedEdge(vA, vD, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vA, ())) |> ignore
        g.AddEdge (TaggedEdge(vB, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vB, vD, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vE, vG, ())) |> ignore
        g.AddEdge (TaggedEdge(vG, vE, ())) |> ignore
        g.AddEdge (TaggedEdge(vE, vH, ())) |> ignore
        g.AddEdge (TaggedEdge(vH, vE, ())) |> ignore
        g.AddEdge (TaggedEdge(vF, vG, ())) |> ignore
        g.AddEdge (TaggedEdge(vG, vF, ())) |> ignore
        g.AddEdge (TaggedEdge(vF, vH, ())) |> ignore
        g.AddEdge (TaggedEdge(vH, vF, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vD, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vD, ())) |> ignore
        g.AddEdge (TaggedEdge(vG, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vG, ())) |> ignore
        g.AddEdge (TaggedEdge(vG, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vG, ())) |> ignore
        g