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

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

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
