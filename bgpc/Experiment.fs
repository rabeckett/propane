module Experiment

let displayHeader () = 
    let headers = 
        ["Pods";
         "Num Nodes"; "Num Edges";
         "Num Prefixes";
         "Size Raw";
         "Size Compressed";
         "Size Percent (Compressed/Raw)"
         "Time Total";
         "Time Join";
         "Time Prefixes (total)";
         "Time Per Prefix (mean)"; "Time Per Prefix (median)"; "Time Per Prefix (max)";
         "Time Per Prefix Build Automaton (mean)"; "Time Per Prefix Build Automaton (median)"; "Time Per Prefix Build Automaton (max)";
         "Time Per Prefix Minimize Automaton (mean)"; "Time Per Prefix Minimize Automaton (med)"; "Time Per Prefix Minimize Automaton (max)";
         "Time Per Prefix Find Ordering (mean)"; "Time Per Prefix Find Ordering (med)"; "Time Per Prefix Find Ordering (max)";
         "Time Per Prefix Gen Config (mean)"; "Time Per Prefix Gen Config (med)"; "Time Per Prefix Gen Config (max)";
         "Time Per Prefix Compress (mean)"; "Time Per Prefix Compress (median)"; "Time Per Prefix Compress (max)"]
    printfn "%s" (Common.List.joinBy "," headers)

let inline toSec v = (float v) / 1000.0
let inline times vs = Array.map toSec vs
let inline mean vs = Array.average vs
let inline maximum vs = Array.max vs

let inline median vs = 
    let sorted = Array.sort vs
    let len = Array.length sorted
    let mid = len / 2
    let median = sorted.[mid]
    if len % 2 = 0 then Array.average [|median; sorted.[mid-1]|] else median

let triple vs = 
    let avg = mean vs 
    let med = median vs 
    let max = maximum vs 
    (avg, med, max)

let displayStats k v e (stats: IR.Stats) =
    let totalTimes = times stats.PerPrefixTimes
    let buildTimes = times stats.PerPrefixBuildTimes
    let minTimes = times stats.PerPrefixMinTimes
    let orderTimes = times stats.PerPrefixOrderTimes
    let genTimes = times stats.PerPrefixGenTimes
    let compressTimes = times stats.PerPrefixCompressTimes
    let (avg, med, max) = triple totalTimes
    let (avgBuild, medBuild, maxBuild) = triple buildTimes
    let (avgMin, medMin, maxMin) = triple minTimes
    let (avgOrd, medOrd, maxOrd) = triple orderTimes
    let (avgGen, medGen, maxGen) = triple minTimes
    let (avgComp, medComp, maxComp) = triple compressTimes
    printfn "%d,%d,%d,%d,%d,%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f" 
        k v e 
        stats.NumPrefixes
        stats.SizeRaw
        stats.SizeCompressed
        (float stats.SizeCompressed / float stats.SizeRaw)
        (toSec stats.TotalTime) 
        (toSec stats.JoinTime) 
        (toSec stats.PrefixTime)
        avg med max
        avgBuild medBuild maxBuild
        avgMin medMin maxMin
        avgOrd medOrd maxOrd
        avgGen medGen maxGen
        avgComp medComp maxComp


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

    let other = 
        let reb = Regex.REBuilder(topo)
        let re1 = reb.Inter [reb.Exit ["IDFX"]; reb.Negate (reb.Enter ["CORE"; "IDFX"])]
        let re2 = reb.Inter [reb.Exit ["CORE"]; reb.Negate (reb.Enter ["CORE"; "IDFX"])]
        reb, [reb.Build re1; reb.Build re2]
  
    pairs <- (Predicate.top, fst other, snd other) :: pairs

    let (ir, _, stats) = IR.compileAllPrefixes "output" topo (List.rev pairs) []
    displayStats k nNodes nEdges stats
    match settings.OutFile with 
    | None -> ()
    | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)

let datacenter () = 
    displayHeader ()
    for k in 4..2..40 do
        System.GC.Collect ()
        singleDatacenter k

