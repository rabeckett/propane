module Benchmark

open Core.Printf
open System.Text

let displayHeader() = 
  let headers = 
    [ "Pods"; "Num Nodes"; "Num Edges"; "Num Prefixes"; "Size Raw"; "Size Compressed"; 
      "Size Percent (Compressed/Raw)"; "Time Total"; "Time Join"; "Time Prefixes (total)"; 
      "Time Per Prefix (mean)"; "Time Per Prefix (median)"; "Time Per Prefix (max)"; 
      "Time Per Prefix Build Automaton (mean)"; "Time Per Prefix Build Automaton (median)"; 
      "Time Per Prefix Build Automaton (max)"; "Time Per Prefix Minimize Automaton (mean)"; 
      "Time Per Prefix Minimize Automaton (med)"; "Time Per Prefix Minimize Automaton (max)"; 
      "Time Per Prefix Find Ordering (mean)"; "Time Per Prefix Find Ordering (med)"; 
      "Time Per Prefix Find Ordering (max)"; "Time Per Prefix Gen Config (mean)"; 
      "Time Per Prefix Gen Config (med)"; "Time Per Prefix Gen Config (max)"; 
      "Time Per Prefix Compress (mean)"; "Time Per Prefix Compress (median)"; 
      "Time Per Prefix Compress (max)" ]
  printfn "%s" (Util.List.joinBy "," headers)

let inline toSec v = (float v) / 1000.0
let inline times vs = Array.map toSec vs
let inline mean vs = Array.average vs
let inline maximum vs = Array.max vs

let inline median vs = 
  let sorted = Array.sort vs
  let len = Array.length sorted
  let mid = len / 2
  let median = sorted.[mid]
  if len % 2 = 0 then 
    Array.average [| median
                     sorted.[mid - 1] |]
  else median

let triple vs = 
  let avg = mean vs
  let med = median vs
  let max = maximum vs
  (avg, med, max)

let displayStats k v e (stats : Abgp.Stats) = 
  let totalTimes = times stats.PerPrefixTimes
  let buildTimes = times stats.PerPrefixBuildTimes
  let minTimes = times stats.PerPrefixMinTimes
  let orderTimes = times stats.PerPrefixOrderTimes
  let genTimes = times stats.PerPrefixGenTimes
  let (avg, med, max) = triple totalTimes
  let (avgBuild, medBuild, maxBuild) = triple buildTimes
  let (avgMin, medMin, maxMin) = triple minTimes
  let (avgOrd, medOrd, maxOrd) = triple orderTimes
  let (avgGen, medGen, maxGen) = triple genTimes
  printfn "%d,%d,%d,%d,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%d" k v e 
    stats.NumPrefixes (toSec stats.JoinTime) (toSec stats.PrefixTime) avg med max avgBuild medBuild 
    maxBuild avgMin medMin maxMin avgOrd medOrd maxOrd avgGen medGen maxGen stats.MinTime

let writeDcPol file 
    ((topo, pfxMap, tierMap) : _ * Topology.Examples.Prefixes * Topology.Examples.Tiers) = 
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
  let sb = StringBuilder()
  // bprintf sb "define T0 = %s\n\n" (Common.List.joinBy " or " t0)
  // bprintf sb "define T1 = %s\n\n" (Common.List.joinBy " or " t1)
  // bprintf sb "define T2 = %s\n\n" (Common.List.joinBy " or " t2)
  bprintf sb "define main = {\n"
  for kv in pfxMap do
    let reb = Regex.REBuilder(topo)
    let prefix = kv.Value
    let loc = kv.Key.Loc
    bprintf sb "  %s => end(%s),\n" (string prefix) loc
  bprintf sb "  true => exit(BACK2) >> exit(BACK1)\n"
  bprintf sb "}\n"
  System.IO.File.WriteAllText(file, string sb)

let writeDcTopo file (topo : Topology.T) = 
  let sb = StringBuilder()
  let mutable asn = 0
  bprintf sb "<topology>\n"
  for n in Topology.vertices topo do
    asn <- asn + 1
    let intern = (string (Topology.isInside n)).ToLower()
    let canOrig = (string (Topology.canOriginateTraffic n)).ToLower()
    bprintf sb "  <node internal=\"%s\" can-originate=\"%s\" asn=\"%s\" name=\"%s\"></node>\n" 
      intern canOrig (string asn) n.Loc
  for (x, y) in Topology.edges topo do
    bprintf sb "  <edge source=\"%s\" target=\"%s\" directed=\"%s\"></edge>\n" x.Loc y.Loc "false"
  bprintf sb "</topology>\n"
  System.IO.File.WriteAllText(file, string sb)

let singleDatacenter outDir k = 
  let (topo, pfxMap, tierMap) = Topology.Examples.fatTree k
  let sep = System.IO.Path.DirectorySeparatorChar
  let filePol = sprintf "%s%cfat%d.pro" outDir sep k
  let fileTopo = sprintf "%s%cfat%d.xml" outDir sep k
  writeDcPol filePol (topo, pfxMap, tierMap)
  writeDcTopo fileTopo topo

let generate() = 
  let dir = "benchmarks"
  System.IO.Directory.CreateDirectory(dir).Create()
  // displayHeader ()
  for k in 4..2..20 do
    singleDatacenter dir k
(*

let singleCore n = 
    let settings = Args.getSettings () 
    let topo = Topology.Examples.complete n
    
    // show customer prefixes
    let nNodes = n
    let nEdges = topo.EdgeCount
    
    // get all cust,peer,paid
    let custs = topo.Vertices |> Seq.filter (fun v -> v.Loc.[0] = 'C') |> Seq.map (fun v -> v.Loc) |> List.ofSeq 
    let peers = topo.Vertices |> Seq.filter (fun v -> v.Loc.[0] = 'P') |> Seq.map (fun v -> v.Loc) |> List.ofSeq 
    let paids = topo.Vertices |> Seq.filter (fun v -> v.Loc.[0] = 'O') |> Seq.map (fun v -> v.Loc) |> List.ofSeq 
    let paidOns = paids |> Seq.filter (fun v -> v.[1] = 'n') |> List.ofSeq 
    let paidOffs = paids |> Seq.filter (fun v -> v.[1] = 'f') |> List.ofSeq 

    // entering preferences
    let getPol (reb: Regex.REBuilder) p = 
        let pp = paids @ peers
        let noTransit = reb.Inter [reb.Any(); reb.Negate (reb.Inter [reb.Enter pp; reb.Exit pp])]
        // let noPeerToPaid = reb.Inter [reb.Any(); reb.Negate (reb.Inter [reb.Through paids; reb.Exit peers] )]
        let pref1 = reb.Inter [reb.Exit custs; noTransit; p]
        let pref2 = reb.Inter [reb.Exit peers; noTransit; p]
        let pref3 = reb.Inter [reb.Exit paidOns; noTransit; p]
        let pref4 = reb.Inter [reb.Exit paidOffs; noTransit; p]
        [reb.Build pref1; reb.Build pref2; reb.Build pref3; reb.Build pref4]
    
    // begin build main policy
    let mutable pairs = []

    // bogon prefix
    let reb = Regex.REBuilder(topo)
    let pol = [reb.Build reb.Empty]
    pairs <- (Predicate.prefix (10u, 0u, 0u, 0u) 16u, reb, pol) :: pairs

    // get regions
    let regions = Seq.splitInto 2 custs
    let mutable i = 10
    for reg in regions do 
        let reb = Regex.REBuilder(topo)
        let pol = reb.Inter[reb.Enter (List.ofArray reg); reb.Exit (List.ofArray reg)]
        pairs <- (Predicate.prefix (uint32 i, 0u,0u,0u) 24u, reb, [reb.Build pol]) :: pairs
        i <- i + 1

    // Main policy 
    let reb = Regex.REBuilder(topo)
    let pol = reb.Any() 
    pairs <- (pb.True, reb, getPol reb pol) :: pairs

    // Compile the policy
    let (ir, _, stats) = Abgp.compileAllPrefixes (settings.DebugDir + "/output") topo (List.rev pairs) []
    displayStats 0 nNodes nEdges stats
    match settings.OutFile with 
    | None -> ()
    | Some out -> System.IO.File.WriteAllText(out + ".ir", Abgp.format ir)

let core () =
    displayHeader () 
    System.GC.Collect ()
    System.Threading.Thread.Sleep(1000)
    for n in 2..3..200 do
        singleCore n
*)