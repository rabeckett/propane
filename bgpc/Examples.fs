﻿module Examples

open QuickGraph
open Topology
open System.Collections.Generic

let topoDisconnected () = 
    let g = BidirectionalGraph<State ,TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=Inside}
    let vB = {Loc="B"; Typ=Inside}
    let vC = {Loc="C"; Typ=Inside}
    let vD = {Loc="D"; Typ=Inside}
    Topology.addVertices g [vA; vB; vC; vD]
    Topology.addEdgesUndirected g [(vA,vB); (vC,vD)]
    g

let topoDiamond () = 
    let g = BidirectionalGraph<State ,TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vX = {Loc="X"; Typ=Inside}
    let vM = {Loc="M"; Typ=Inside}
    let vN = {Loc="N"; Typ=Inside}
    let vY = {Loc="Y"; Typ=Inside}
    let vZ = {Loc="Z"; Typ=Inside}
    let vB = {Loc="B"; Typ=InsideOriginates}
    Topology.addVertices g [vA; vX; vM; vN; vY; vZ; vB]
    Topology.addEdgesUndirected g [(vA,vX); (vA,vM); (vM,vN); (vX,vN); (vN,vY); (vN,vZ); (vY,vB); (vZ,vB)]
    g

let topoDatacenterSmall () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vB = {Loc="B"; Typ=InsideOriginates}
    let vC = {Loc="C"; Typ=InsideOriginates}
    let vD = {Loc="D"; Typ=InsideOriginates}
    let vX = {Loc="X"; Typ=Inside}
    let vY = {Loc="Y"; Typ=Inside}
    let vM = {Loc="M"; Typ=Inside}
    let vN = {Loc="N"; Typ=Inside}
    Topology.addVertices g [vA; vB; vC; vD; vX; vY; vM; vN]
    Topology.addEdgesUndirected g [(vA,vX); (vB,vX); (vC,vY); (vD,vY); (vX,vM); (vX,vN); (vY,vM); (vY,vN)]
    g

let topoDatacenterMedium () = 
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
    Topology.addVertices g [vA; vB; vC; vD; vE; vF; vG; vH; vX; vY]
    Topology.addEdgesUndirected g 
        [(vA,vC); (vA,vD); (vB,vC); (vB,vD); (vE,vG); (vE,vH); (vF,vG); (vF,vH); 
         (vC,vX); (vC,vY); (vD,vX); (vD,vY); (vG,vX); (vG,vY); (vH,vX); (vH,vY)]
    g

let topoDatacenterMediumAggregation () = 
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
    let vPeer = {Loc="PEER"; Typ=Outside}
    Topology.addVertices g [vA; vB; vC; vD; vE; vF; vG; vH; vX; vY; vPeer]
    Topology.addEdgesUndirected g 
        [(vA,vC); (vA,vD); (vB,vC); (vB,vD); (vE,vG); (vE,vH); (vF,vG); (vF,vH); 
         (vC,vX); (vC,vY); (vD,vX); (vD,vY); (vG,vX); (vG,vY); (vH,vX); (vH,vY);
         (vX, vPeer); (vY, vPeer)]
    g

let topoDatacenterLarge () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vB = {Loc="B"; Typ=InsideOriginates}
    let vC = {Loc="C"; Typ=InsideOriginates}
    let vD = {Loc="D"; Typ=InsideOriginates}
    let vE = {Loc="E"; Typ=InsideOriginates}
    let vF = {Loc="F"; Typ=InsideOriginates}
    let vM = {Loc="M"; Typ=Inside}
    let vN = {Loc="N"; Typ=Inside}
    let vO = {Loc="O"; Typ=Inside}
    let vX = {Loc="X"; Typ=Inside}
    let vY = {Loc="Y"; Typ=Inside}
    let vZ = {Loc="Z"; Typ=Inside}
    Topology.addVertices g [vA; vB; vC; vD; vE; vF; vM; vN; vO; vX; vY; vZ]
    Topology.addEdgesUndirected g 
        [(vA, vX); (vB, vX); (vC, vY); (vD, vY); (vE, vZ); (vF, vZ); 
         (vX, vM); (vX, vN); (vX, vO); (vY, vM); (vY, vN); (vY, vO);
         (vZ, vM); (vZ, vN); (vZ, vO)]
    g

let topoBadGadget () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vB = {Loc="B"; Typ=InsideOriginates}
    let vC = {Loc="C"; Typ=InsideOriginates}
    let vD = {Loc="D"; Typ=InsideOriginates}
    Topology.addVertices g [vA; vB; vC; vD]
    Topology.addEdgesUndirected g [(vA,vB); (vB,vC); (vC,vA); (vA,vD); (vB,vD); (vC,vD)]
    g

let topoBrokenTriangle () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vB = {Loc="B"; Typ=Inside}
    let vC = {Loc="C"; Typ=InsideOriginates}
    let vD = {Loc="D"; Typ=InsideOriginates}
    let vE = {Loc="E"; Typ=Inside}
    Topology.addVertices g [vA; vB; vC; vD; vE]
    Topology.addEdgesUndirected g [(vC,vA); (vA,vE); (vA,vB); (vE,vD); (vD,vB)]
    g

let topoBigDipper () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vC = {Loc="C"; Typ=InsideOriginates}
    let vD = {Loc="D"; Typ=InsideOriginates}
    let vE = {Loc="E"; Typ=Inside}
    Topology.addVertices g [vA; vC; vD; vE]
    Topology.addEdgesUndirected g [(vC,vA); (vA,vE); (vA,vD); (vE,vD)]
    g

let topoSeesaw () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vM = {Loc="M"; Typ=InsideOriginates}
    let vN = {Loc="N"; Typ=Inside}
    let vO = {Loc="O"; Typ=Inside}
    let vX = {Loc="X"; Typ=InsideOriginates}
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vB = {Loc="B"; Typ=InsideOriginates}
    Topology.addVertices g [vM; vN; vO; vX; vA; vB]
    Topology.addEdgesUndirected g [(vM, vN); (vM, vO); (vO, vX); (vN, vX); (vX, vA); (vX, vB)]
    g

let topoStretchingManWAN () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=Inside}
    let vB = {Loc="B"; Typ=Inside}
    let vC = {Loc="C"; Typ=Inside}
    let vD = {Loc="D"; Typ=Inside}
    let vX = {Loc="X"; Typ=Outside}
    let vY = {Loc="Y"; Typ=Outside}
    let vZ = {Loc="Z"; Typ=Outside}
    Topology.addVertices g [vA; vB; vC; vD; vX; vY; vZ]
    Topology.addEdgesUndirected g [(vX, vA); (vX, vB); (vA, vC); (vB, vC); (vC, vD); (vD, vY); (vD, vZ)]
    g

let topoStretchingManWAN2 () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=Inside}
    let vB = {Loc="B"; Typ=Inside}
    let vC = {Loc="C"; Typ=Inside}
    let vD = {Loc="D"; Typ=Inside}
    let vE = {Loc="E"; Typ=Inside}
    let vW = {Loc="W"; Typ=Outside}
    let vX = {Loc="X"; Typ=Outside}
    let vY = {Loc="Y"; Typ=Outside}
    let vZ = {Loc="Z"; Typ=Outside}
    Topology.addVertices g [vA; vB; vC; vD; vE; vW; vX; vY; vZ]
    Topology.addEdgesUndirected g 
        [(vW,vA); (vW,vB); (vA,vC); (vB,vC); (vC, vD); 
         (vC, vE); (vD, vX); (vD, vY); (vE, vZ)]
    g

let topoPinCushionWAN () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=Inside}
    let vB = {Loc="B"; Typ=Inside}
    let vC = {Loc="C"; Typ=Inside}
    let vD = {Loc="D"; Typ=Inside}
    let vE = {Loc="E"; Typ=Inside}
    let vW = {Loc="W"; Typ=Outside}
    let vX = {Loc="X"; Typ=Outside}
    let vY = {Loc="Y"; Typ=Outside}
    let vZ = {Loc="Z"; Typ=Outside}
    Topology.addVertices g [vA; vB; vC; vD; vE; vW; vX; vY; vZ]
    Topology.addEdgesUndirected g [(vW, vA); (vX, vB); (vA, vC); (vB, vC); (vC, vD); (vC, vE); (vD, vY); (vE, vZ)]
    g

let topoBackboneWAN () = 
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let vA = {Loc="A"; Typ=InsideOriginates}
    let vSEA = {Loc="SEA"; Typ=InsideOriginates}
    let vNY = {Loc="NY"; Typ=InsideOriginates}
    let vX = {Loc="X"; Typ=Outside}
    let vY = {Loc="Y"; Typ=Outside}
    Topology.addVertices g [vA; vSEA; vNY; vX; vY]
    Topology.addEdgesUndirected g [(vA, vSEA); (vA, vNY); (vSEA, vX); (vNY, vY)]
    g

/// Fattree topology 

type Tiers = Dictionary<State,int>
type Prefixes = Dictionary<State,Prefix.T>

let getPrefix i = 
    let a = uint32 (i / (256 * 256))
    let b = uint32 (i / 256)
    let c = uint32 (i % 256)
    Prefix.prefix (a, b, c, 0u) 24u

let fatTree k : T * Prefixes * Tiers = 
    let iT0 = (k * k) / 2
    let iT1 = (k * k) / 2
    let iT2 = (k * k) / 4
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
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
    (g, prefixes, tiers)