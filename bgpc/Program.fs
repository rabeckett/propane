
open Extension.Error

let RE = Regex.REBuilder(Set.ofList ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "X"; "Y"], Set.empty)

[<EntryPoint>]
let main argv = 
    let inStar = RE.Star RE.Inside
    let r1 = (RE.Concat (RE.Concat inStar (RE.Loc "X")) inStar)
    let r2 = (RE.Concat (RE.Concat inStar (RE.Loc "Y")) inStar)

    let dfa1 = RE.MakeDFA 1 (RE.Rev r1)
    let dfa2 = RE.MakeDFA 2 (RE.Rev r2)
    (* let dfa3 = RE.MakeDFA 3 (RE.Rev x) *)
    
    let cg = CGraph.build (Topology.Example3.topo()) [|dfa1; dfa2|] 
    
    (* Minimize.pruneHeuristic cg *)
    
    Minimize.removeEdgesForDominatedNodes cg
    Minimize.removeNodesNotOnAnySimplePathToEnd cg
    
    printfn "%s" (CGraph.toDot cg)

    (* match CGraph.compile cg with 
    | Ok(config) -> Config.print config
    | Err(CGraph.PrefConsistency (x,y)) -> failwith "pref consistency"
    | Err(CGraph.TopoConsistency (x,y)) -> failwith "topo consistency" *)

    0