
open Extension.Error

let RE = Regex.REBuilder(Set.ofList ["A"; "B"; "C"; "D"; "X"; "Y"; "M"; "N"], Set.empty)

[<EntryPoint>]
let main argv = 
    let x = RE.Star RE.Inside
    (* let r1 = RE.Concat (RE.Concat x (RE.Loc "M")) x
    let r2 = RE.Concat (RE.Concat x (RE.Loc "N")) x *)

    let dfa1 = RE.MakeDFA 1 (RE.Rev x)
    (* let dfa2 = RE.MakeDFA 2 (RE.Rev r2) *)
    
    let cg = CGraph.build (Topology.Example2.topo()) [|dfa1|] 
    CGraph.pruneHeuristic cg

    printfn "%s" (CGraph.toDot cg)

    (* match CGraph.compile cg with 
    | Ok(config) -> Config.print config
    | Err(CGraph.PrefConsistency (x,y)) -> failwith "pref consistency"
    | Err(CGraph.TopoConsistency (x,y)) -> failwith "topo consistency" *)

    0