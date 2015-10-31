
open Extension.Error

let topo = Topology.Example3.topo ()

let RE = Regex.REBuilder(topo)

[<EntryPoint>]
let main argv = 
    let inStar = RE.Star RE.Inside
    let r1 = (RE.Concat (RE.Concat inStar (RE.Loc "X")) inStar)
    let r2 = (RE.Concat (RE.Concat inStar (RE.Loc "Y")) inStar)

    let dfa1 = RE.MakeDFA (RE.Rev r1)
    let dfa2 = RE.MakeDFA (RE.Rev r2)
    (* let dfa3 = RE.MakeDFA 3 (RE.Rev inStar) *)
    
    let cg = CGraph.build topo [|dfa1; dfa2|] 
    

    (* Minimize.pruneHeuristic cg *)
   
    Minimize.removeEdgesForDominatedNodes cg
    Minimize.removeNodesNotOnAnySimplePathToEnd cg
    

    (* printfn "%s" (CGraph.toDot cg) *)

    match Config.compile topo cg with
    | Ok(config) -> Config.print config
    | Err(Consistency.PrefViolation (x,y)) ->
        printfn "Pref Consistency Violation" 
        printfn "Node1: %A" x 
        printfn "Node2: %A" y
    | Err(Consistency.TopoViolation (x,y)) -> failwith "topo consistency"

    0