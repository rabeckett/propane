module Experiment

let displayHeader () = 
    let headers = 
        ["Pods";
         "Num Nodes"; "Num Edges";
         "Total Time";
         "Join Time";
         "Num Prefixes";
         "Prefix Time (total)";
         "Per Prefix (mean)"; "Per Prefix (median)"; "Per Prefix (max)"]
    printfn "%s" (Common.List.joinBy "," headers)

let displayStats k v e (stats: IR.Stats) =
    let times = Array.map (fun t -> (float t) / 1000.0) stats.PerPrefixTimes
    let mean = Array.average times
    let sorted = Array.sort times
    let len = Array.length sorted
    let mid = len / 2
    let median = sorted.[mid]
    let median = if len % 2 = 0 then Array.average [|median; sorted.[mid-1]|] else median
    let max = Array.max times
    printfn "%d,%d,%d,%f,%f,%d,%f,%f,%f,%f" k v e 
        (float stats.TotalTime / 1000.0) 
        ((float stats.JoinTime) / 1000.0) 
        stats.NumPrefixes 
        ((float stats.PrefixTime) / 1000.0)
        mean median max

let singleDatacenter k =
    let settings = Args.getSettings () 
    let (topo, pfxMap, tierMap) = Topology.Examples.fatTree k
    let nNodes = Seq.length topo.Vertices 
    let nEdges = Seq.length topo.Edges
    let mutable t0 = []
    let mutable t1 = []
    let mutable t2 = []
    for kv in tierMap do 
        let loc = kv.Key.Loc
        let tier = kv.Value
        match tier with 
        | 0 -> t0 <- loc :: t0
        | 1 -> t1 <- loc :: t1
        | 2 -> t2 <- loc :: t2
        | _ -> failwith "invalid tier"
    let yammer =
        let reb = Regex.REBuilder(topo)
        let re = reb.Inter [reb.Only (reb.Locs (t0 @ t1))]
        (reb, reb.Build re)
    let block = 
        let reb = Regex.REBuilder(topo)
        let re = reb.Avoid ["CORE"; "IDFX"]
        (reb, reb.Build re)
    let reuse = 
        let reb = Regex.REBuilder(topo)
        let re = reb.Avoid ["IDFX"]
        (reb, reb.Build re)

    let mutable pairs = [
        (Predicate.prefix (10u, 0u, 0u, 0u) 16u, fst reuse, [snd reuse]); 
        (Predicate.community "BLOCK", fst block, [snd block]); 
        (Predicate.community "YAMMER", fst yammer, [snd yammer])
    ]
    
    for kv in pfxMap do 
        let reb = Regex.REBuilder(topo)
        let prefix = kv.Value 
        let loc = kv.Key.Loc 
        let pred = Predicate.prefixPred (Prefix.toPredicate [prefix])
        let re = reb.End [loc]
        pairs <- (pred, reb, [reb.Build re]) :: pairs

    (* let other = 
        let reb = Regex.REBuilder(topo)
        let re1 = reb.Inter [reb.Exit ["IDFX"]; reb.Negate (reb.Enter ["CORE"; "IDFX"])]
        let re2 = reb.Inter [reb.Exit ["CORE"]; reb.Negate (reb.Enter ["CORE"; "IDFX"])]
        reb, [reb.Build re1; reb.Build re2]
    
    pairs <- [(Predicate.top, fst other, snd other)] *)

    let (ir, stats) = IR.compileAllPrefixes "output" topo (List.rev pairs) []
    displayStats k nNodes nEdges stats
    match settings.OutFile with 
    | None -> ()
    | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)

let datacenter () = 
    displayHeader ()
    for i in 2..17 do
        singleDatacenter (i*2)

