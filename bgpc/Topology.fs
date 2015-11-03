module Topology

open QuickGraph


type NodeType = 
    | Start
    | End
    | Outside
    | Inside 
    | InsideOriginates

type State = 
    {Loc: string; 
     Typ: NodeType}

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

/// Build the internal and external alphabet from a topology
let alphabet (topo: T) : Set<State> * Set<State> = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.Typ with 
        | Inside -> ain <- Set.add v ain
        | InsideOriginates -> ain <- Set.add v ain
        | Outside -> aout <- Set.add v aout
        | Start | End -> failwith "unreachable"
    (ain, aout)

/// Check if a node is a valid topology node
let isTopoNode (t: State) = 
    match t.Typ with 
    | Start | End -> false
    | Outside | Inside | InsideOriginates -> true

/// Check if a node represents an internal location (under AS control)
let isInside (t: State) = 
    match t.Typ with 
    | Inside | InsideOriginates ->  true 
    | Outside | Start | End -> false

/// Check if a node can originate traffice (e.g., TOR in DC)
let canOriginateTraffic (t: State) = 
    match t.Typ with 
    | InsideOriginates -> true 
    | Outside -> true
    | Inside -> false
    | Start | End -> false

/// Checks if a topology is well-formed. This involves checking 
/// for duplicate names, as well as checking that the inside is fully connected
let isWellFormed (t: State) = 
    false


/// Helper module for enumerating and constructing topology failure scenarios
module Failure = 

    type FailType = 
        | NodeFailure of State 
        | LinkFailure of TaggedEdge<State,unit>

    let allFailures n (topo: T) : seq<FailType list> = 
        let fvs = topo.Vertices |> Seq.filter isInside |> Seq.map NodeFailure
        let fes = 
            topo.Edges 
            |> Seq.filter (fun e -> isInside e.Source || isInside e.Target) 
            |> Seq.map LinkFailure 
        Seq.append fes fvs 
        |> Seq.toList 
        |> Extension.List.combinations n


module Example1 = 
    let topo () = 
        let g = BidirectionalGraph<State ,TaggedEdge<State,unit>>()
        let vA = {Loc="A"; Typ=InsideOriginates}
        let vX = {Loc="X"; Typ=Inside}
        let vM = {Loc="M"; Typ=Inside}
        let vN = {Loc="N"; Typ=Inside}
        let vY = {Loc="Y"; Typ=Inside}
        let vZ = {Loc="Z"; Typ=Inside}
        let vB = {Loc="B"; Typ=InsideOriginates}
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
        let vA = {Loc="A"; Typ=InsideOriginates}
        let vB = {Loc="B"; Typ=InsideOriginates}
        let vC = {Loc="C"; Typ=InsideOriginates}
        let vD = {Loc="D"; Typ=InsideOriginates}
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
        let vA = {Loc="A"; Typ=InsideOriginates}
        let vB = {Loc="B"; Typ=InsideOriginates}
        let vC = {Loc="C"; Typ=Inside}
        let vD = {Loc="D"; Typ=Inside}
        let vE = {Loc="E"; Typ=InsideOriginates}
        let vF = {Loc="F"; Typ=InsideOriginates}
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
        g.AddEdge (TaggedEdge(vH, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vH, ())) |> ignore
        g.AddEdge (TaggedEdge(vH, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vH, ())) |> ignore
        g