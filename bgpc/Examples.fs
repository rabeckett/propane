module Examples
open QuickGraph
open Topology

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


let dataCenter (tiers: (int*int) list) =
    let loc t i = 
        "T" + string t + "_" + string i 
    // generate and store new prefixes as we go
    let currPrefix = ref 0
    let prefixMap = ref Map.empty
    // remeber what tier nodes are in
    let tierMap = ref Map.empty
    // build a topology from tiers of a datacenter
    let g = BidirectionalGraph<State, TaggedEdge<State,unit>>()
    let maxTier = (List.length tiers) - 1
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
                    let v = {Loc=l; Typ=if currTier=0 then Topology.InsideOriginates else Topology.Inside}
                    g.AddVertex v |> ignore
                    tierMap := Map.add v currTier !tierMap
                    if currTier = 0 then 
                        let a = uint32 (!currPrefix % 256 * 256 * 256)
                        let b = uint32 (!currPrefix % 256 * 256)
                        let c = uint32 (!currPrefix % 256)
                        let p = Prefix.prefix (a, b, c, 0u) 24u
                        prefixMap := Map.add v p !prefixMap
                        currPrefix := !currPrefix + 1
                    for u in parents do
                        printfn "Adding edge between: %s and %s" v.Loc u.Loc
                        g.AddEdge (TaggedEdge(v,u,())) |> ignore
                        g.AddEdge (TaggedEdge(u,v,())) |> ignore
                    newParents <- v :: newParents
                if currTier = 0 then () 
                else aux (currTier - 1) b newParents tl
    aux maxTier 0 [] (List.rev tiers)
    (g, !prefixMap, !tierMap)