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
      stats.NumPrefixes (toSec stats.JoinTime) (toSec stats.PrefixTime) avg med max avgBuild 
      medBuild maxBuild avgMin medMin maxMin avgOrd medOrd maxOrd avgGen medGen maxGen stats.MinTime

let fattreeAbstract = """
  <!-- Abstract Nodes -->
  <abstractnode internal="true" label="T0"></abstractnode>
  <abstractnode internal="true" label="T1"></abstractnode>
  <abstractnode internal="true" label="T2"></abstractnode>
  <abstractnode internal="false" label="Peer1"></abstractnode>
  <abstractnode internal="false" label="Peer2"></abstractnode>
  <!-- Abstract Edges -->
  <abstractedge source="T0" target="T1" labels="(E1,E2)"></abstractedge>
  <abstractedge source="T1" target="T2" labels="(E3,E4)"></abstractedge>
  <abstractedge source="T2" target="Peer1" labels="(E5,E6)"></abstractedge>
  <abstractedge source="T2" target="Peer2" labels="(E7,E8)"></abstractedge>
  <!-- Constraints -->
  <constraint assertion="(>= T0 4)"></constraint>
  <constraint assertion="(>= T1 4)"></constraint>
  <constraint assertion="(>= T2 2)"></constraint>
  <constraint assertion="(>= E1 T1)"></constraint>
  <constraint assertion="(>= E2 T0)"></constraint>
  <constraint assertion="(>= E3 T2)"></constraint>
  <constraint assertion="(>= E4 T1)"></constraint>
  <constraint assertion="(>= E5 Peer1)"></constraint>
  <constraint assertion="(>= E6 T2)"></constraint>
  <constraint assertion="(>= E7 Peer2)"></constraint>
  <constraint assertion="(>= E8 T2)"></constraint>
  <!-- Template Variables -->
  <data vars="aggregatePrefix=0.0.0.0/16"></data>
"""

let writeDcPolConcrete ((topo, pfxMap, tierMap) : _ * Topology.Examples.Prefixes * Topology.Examples.Tiers) = 
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
   bprintf sb "define main = {\n"
   for kv in pfxMap do
      let reb = Regex.REBuilder(topo)
      let prefix = kv.Value
      let loc = kv.Key.Loc
      bprintf sb "  %s => end(%s),\n" (string prefix) loc
   bprintf sb "  true => exit(Peer1 >> Peer2)\n"
   bprintf sb "}\n"
   bprintf sb "\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(0.0.0.0/16, in -> out)\n"
   bprintf sb "}\n"
   string sb

let writeDcPolAbstract() = 
   let sb = StringBuilder()
   bprintf sb "define main = {\n"
   bprintf sb "  T0.$prefix$ => end(T0),\n"
   bprintf sb "  true => exit(Peer1 >> Peer2)\n"
   bprintf sb "}\n"
   bprintf sb "\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate($aggregatePrefix$, in -> out)\n"
   bprintf sb "}\n"
   string sb

let writeDcTopoConcrete (topo : Topology.T) = 
   let sb = StringBuilder()
   let mutable asn = 1
   bprintf sb "<topology asn=\"100\">\n"
   for n in Topology.vertices topo do
      asn <- asn + 1
      let b = Topology.isInside n
      let intern = (string b).ToLower()
      bprintf sb "  <node internal=\"%s\" asn=\"%s\" name=\"%s\"></node>\n" intern (string asn) 
         n.Loc
   for (x, y) in Topology.edges topo do
      bprintf sb "  <edge source=\"%s\" target=\"%s\"></edge>\n" x.Loc y.Loc
   bprintf sb "</topology>\n"
   string sb

let writeDcTopoAbstract ((topo, pfxMap, tierMap) : Topology.T * Topology.Examples.Prefixes * Topology.Examples.Tiers) = 
   let sb = StringBuilder()
   let mutable asn = 1
   bprintf sb "<topology asn=\"100\">\n"
   for n in Topology.vertices topo do
      asn <- asn + 1
      let b = Topology.isInside n
      let intern = (string b).ToLower()
      
      let group = 
         if tierMap.ContainsKey n then sprintf " group=\"%s\"" ("T" + string tierMap.[n])
         else if n.Loc = "Peer1" then " group=\"Peer1\""
         else if n.Loc = "Peer2" then " group=\"Peer2\""
         else ""
      
      let vars = 
         if pfxMap.ContainsKey n then sprintf " vars=\"prefix=%s\"" (string pfxMap.[n])
         else ""
      
      bprintf sb "  <node internal=\"%s\" asn=\"%s\" name=\"%s\"%s%s></node>\n" intern (string asn) 
         n.Loc group vars
   for (x, y) in Topology.edges topo do
      bprintf sb "  <edge source=\"%s\" target=\"%s\"></edge>\n" x.Loc y.Loc
   bprintf sb "%s" fattreeAbstract
   bprintf sb "</topology>\n"
   string sb

let singleDatacenter outDir k = 
   let (topo, pfxMap, tierMap) = Topology.Examples.fatTree k
   let sep = System.IO.Path.DirectorySeparatorChar
   // concrete topology
   let cFilePol = sprintf "%s%cfat%d.pro" outDir sep k
   let cPol = writeDcPolConcrete (topo, pfxMap, tierMap)
   let cFileTopo = sprintf "%s%cfat%d.xml" outDir sep k
   let cTopo = writeDcTopoConcrete topo
   System.IO.File.WriteAllText(cFilePol, cPol)
   System.IO.File.WriteAllText(cFileTopo, cTopo)
   // abstract topology
   let aFilePol = sprintf "%s%cfat%d_abs.pro" outDir sep k
   let aPol = writeDcPolAbstract()
   let aFileTopo = sprintf "%s%cfat%d_abs.xml" outDir sep k
   let aTopo = writeDcTopoAbstract (topo, pfxMap, tierMap)
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo)

let generate() = 
   let dir = "benchmarks"
   Util.File.createDir dir
   for k in 4..2..32 do
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