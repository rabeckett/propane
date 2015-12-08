module Examples

open QuickGraph
open Topology


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


(* TODO: add examples with external ASes *)