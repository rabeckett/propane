module Benchmark

open Core.Printf
open System.Text
open Topology
open Util

let fattreeAbstract k = sprintf """
  <!-- Abstract Nodes -->
  <abstractnode internal="true" label="T0L"></abstractnode>
  <abstractnode internal="true" label="T0G"></abstractnode>
  <abstractnode internal="true" label="T1"></abstractnode>
  <abstractnode internal="true" label="T2"></abstractnode>
  <abstractnode internal="false" label="Peer1"></abstractnode>
  <abstractnode internal="false" label="Peer2"></abstractnode>
  <!-- Abstract Edges -->
  <abstractedge source="T0L" target="T1" labels="(E1,E2)"></abstractedge>
  <abstractedge source="T0G" target="T1" labels="(E9,E10)"></abstractedge>
  <abstractedge source="T1" target="T2" labels="(E3,E4)"></abstractedge>
  <abstractedge source="T2" target="Peer1" labels="(E5,E6)"></abstractedge>
  <abstractedge source="T2" target="Peer2" labels="(E7,E8)"></abstractedge>
  <!-- Abstract Pods -->
  <abstractpod label="P">
     <element>T0L</element>
     <element>T0G</element>
     <element>T1</element>
  </abstractpod>
  <!-- Constraints -->
  <constraint assertion="(>= T0L %d)"></constraint>
  <constraint assertion="(>= T0G %d)"></constraint>
  <constraint assertion="(>= T1 %d)"></constraint>
  <constraint assertion="(>= T2 %d)"></constraint>
  <constraint assertion="(>= E1 T1)"></constraint>
  <constraint assertion="(>= E9 T1)"></constraint>
  <constraint assertion="(>= E2 T0L)"></constraint>
  <constraint assertion="(>= E10 T0G)"></constraint>
  <constraint assertion="(>= E3 T2)"></constraint>
  <constraint assertion="(>= E4 1)"></constraint>
  <constraint assertion="(>= E5 Peer1)"></constraint>
  <constraint assertion="(>= E6 T2)"></constraint>
  <constraint assertion="(>= E7 Peer2)"></constraint>
  <constraint assertion="(>= E8 T2)"></constraint>
  <!-- Template Variables -->
  <data vars="aggregatePrefix=0.0.0.0/16"></data>""" (k / 4) (k / 4) (k / 2) ((k / 2) * (k / 2))
let coreAbstract n = sprintf """
  <!-- Abstract Nodes -->
  <abstractnode internal="true" label="Core"></abstractnode>
  <abstractnode internal="true" label="Border"></abstractnode>
  <abstractnode internal="false" label="Cust"></abstractnode>
  <abstractnode internal="false" label="Peer"></abstractnode>
  <abstractnode internal="false" label="OnPaid"></abstractnode>
  <abstractnode internal="false" label="OffPaid"></abstractnode>
  <!-- Abstract Edges -->
  <abstractedge source="Border" target="Cust" labels="(E1,E2)"></abstractedge>
  <abstractedge source="Border" target="Peer" labels="(E3,E4)"></abstractedge>
  <abstractedge source="Border" target="OnPaid" labels="(E5,E6)"></abstractedge>
  <abstractedge source="Border" target="OffPaid" labels="(E7,E8)"></abstractedge>
  <abstractedge source="Core" target="Core" labels="(E9,E9)"></abstractedge>
  <abstractedge source="Core" target="Border" labels="(E10,E11)"></abstractedge>
  <!-- Constraints -->
  <constraint assertion="(= E1 Cust)"></constraint>
  <constraint assertion="(= E3 Peer)"></constraint>
  <constraint assertion="(= E5 OnPaid)"></constraint>
  <constraint assertion="(= E7 OffPaid)"></constraint>
  <constraint assertion="(= E9 Core)"></constraint>
  <constraint assertion="(>= E11 2)"></constraint>
  <constraint assertion="(>= Core %d)"></constraint>""" (n / 2)
let definitions = """
define bogon = 
  100.64.0.0/[10..32] or
  127.0.0.0/[8..32] or
  192.0.0.0/[24..32] or
  192.0.2.0/[24..32] or
  198.18.0.0/[15..32] or
  198.51.100.0/[24..32] or
  203.0.113.0/[24..32] or
  224.0.0.0/[3..32] 

define private = 
  10.0.0.0/[8..32] or 
  172.16.0.0/[12..32] or 
  192.168.0.0/[16..32] or 
  169.254.0.0/[16..32]

define transit(X,Y) = enter(X+Y) & exit(X+Y)

"""

let writeDcPolConcrete ((topo, pfxMap, tierMap) : _ * Examples.Prefixes * Examples.Tiers) = 
   let sb = StringBuilder()
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(out,out)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   for kv in pfxMap do
      let prefix = kv.Value
      let loc = kv.Key.Loc
      let pfx = string prefix
      
      let str = 
         if pfx.[2] = '1' then " & always(in)"
         else ""
      bprintf sb "  %s => end(%s)%s,\n" pfx loc str
   bprintf sb "  true => exit(Peer1 >> Peer2)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(0.0.0.0/16, in -> out)\n"
   bprintf sb "}\n"
   string sb

let writeDcPolAbstract() = 
   let sb = StringBuilder()
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(out,out)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   bprintf sb "  T0G.$prefix$ => end(T0G),\n"
   bprintf sb "  T0L.$prefix$ => end(T0L) & always(in),\n"
   bprintf sb "  true => exit(Peer1 >> Peer2)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n"
   bprintf sb "\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate($aggregatePrefix$, in -> out)\n"
   bprintf sb "}\n"
   string sb

let writeTopologyConcrete (topo : Topology.T) = 
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

let writeDcTopoAbstract ((topo, pfxMap, tierMap) : Topology.T * Topology.Examples.Prefixes * Topology.Examples.Tiers) 
    k = 
   let sb = StringBuilder()
   let mutable asn = 1
   bprintf sb "<topology asn=\"100\">\n"
   for n in Topology.vertices topo do
      asn <- asn + 1
      let b = Topology.isInside n
      let intern = (string b).ToLower()
      
      let group = 
         if tierMap.ContainsKey n then 
            let ext = 
               if tierMap.[n] = 0 then 
                  let pfx = string pfxMap.[n]
                  if pfx.[2] = '1' then "L"
                  else "G"
               else ""
            sprintf " group=\"%s\"" ("T" + string tierMap.[n] + ext)
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
   bprintf sb "%s" (fattreeAbstract k)
   bprintf sb "</topology>\n"
   string sb

(* let writeBackbonePolConcrete (topo : Topology.T) = 
   let (custs, peers, onpaids, offpaids) = getPeerGroups topo
   let sb = StringBuilder()
   bprintf sb "define Cust = %s\n" (Util.Set.toString custs)
   bprintf sb "define Peer = %s\n" (Util.Set.toString peers)
   bprintf sb "define OnPaid = %s\n" (Util.Set.toString onpaids)
   bprintf sb "define OffPaid = %s\n" (Util.Set.toString offpaids)
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(NonCust, NonCust)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   bprintf sb "  1.1.1.1 => end(R0),\n"
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(1.1.0.0/16, in -> out)"
   bprintf sb "}"
   string sb

let writeBackbonePolAbstract (topo : Topology.T) = 
   let sb = StringBuilder()
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(NonCust, NonCust)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   bprintf sb "  1.1.1.1 => end(Inside),\n"
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(1.1.0.0/16, in -> out)\n"
   bprintf sb "}"
   string sb

let writeBackboneTopoAbstract (topo : Topology.T) n = 
   let sb = StringBuilder()
   let (custs, peers, onpaids, offpaids) = getPeerGroups topo
   let mutable asn = 1
   bprintf sb "<topology asn=\"100\">\n"
   for n in Topology.vertices topo do
      asn <- asn + 1
      let b = Topology.isInside n
      let intern = (string b).ToLower()
      
      let group = 
         if Set.contains n.Loc custs then " group=\"Cust\""
         else if Set.contains n.Loc peers then " group=\"Peer\""
         else if Set.contains n.Loc onpaids then " group=\"OnPaid\""
         else if Set.contains n.Loc offpaids then " group=\"OffPaid\""
         else " group=\"Inside\""
      bprintf sb "  <node internal=\"%s\" asn=\"%s\" name=\"%s\"%s></node>\n" intern (string asn) 
         n.Loc group
   for (x, y) in Topology.edges topo do
      bprintf sb "  <edge source=\"%s\" target=\"%s\"></edge>\n" x.Loc y.Loc
   bprintf sb "%s" (backboneAbstract n)
   bprintf sb "</topology>\n"
   string sb *)

let getPeerGroups topo = 
   let mutable custs = Set.empty
   let mutable peers = Set.empty
   let mutable onpaids = Set.empty
   let mutable offpaids = Set.empty
   let mutable borders = Set.empty
   let mutable cores = Set.empty
   for v in Topology.vertices topo do
      let l = v.Loc
      if l.StartsWith("Cust") then custs <- Set.add l custs
      if l.StartsWith("Peer") then peers <- Set.add l peers
      if l.StartsWith("OnPaid") then onpaids <- Set.add l onpaids
      if l.StartsWith("OffPaid") then offpaids <- Set.add l offpaids
      if l.StartsWith("Border") then borders <- Set.add l borders
      if l.StartsWith("Core") then cores <- Set.add l cores
   (custs, peers, onpaids, offpaids, borders, cores)

let writeCorePolConcrete ((topo, pfxMap) : T * Examples.Prefixes) = 
   let sb = StringBuilder()
   let (custs, peers, onpaids, offpaids, _, _) = getPeerGroups topo
   bprintf sb "define Cust = %s\n" (Util.Set.toString custs)
   bprintf sb "define Peer = %s\n" (Util.Set.toString peers)
   bprintf sb "define OnPaid = %s\n" (Util.Set.toString onpaids)
   bprintf sb "define OffPaid = %s\n" (Util.Set.toString offpaids)
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(NonCust, NonCust)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   for kv in pfxMap do
      let prefix = kv.Value
      let loc = kv.Key.Loc
      bprintf sb "  %s => end(%s),\n" (string prefix) loc
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(0.0.0.0/16, in -> out)\n"
   bprintf sb "}\n"
   string sb

let writeCorePolAbstract (topo : Topology.T) = 
   let sb = StringBuilder()
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not transit(NonCust, NonCust)\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop,\n"
   bprintf sb "  private => drop,\n"
   bprintf sb "  Core.$prefix$ => end(Core),\n"
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n\n"
   bprintf sb "control {\n"
   bprintf sb "  aggregate(1.1.0.0/16, in -> out)\n"
   bprintf sb "}"
   string sb

let writeCoreTopoAbstract ((topo, pfxMap) : T * Examples.Prefixes) n = 
   let sb = StringBuilder()
   let (custs, peers, onpaids, offpaids, borders, cores) = getPeerGroups topo
   let mutable asn = 1
   bprintf sb "<topology asn=\"100\">\n"
   for n in Topology.vertices topo do
      asn <- asn + 1
      let b = Topology.isInside n
      let intern = (string b).ToLower()
      
      let group = 
         if Set.contains n.Loc custs then " group=\"Cust\""
         else if Set.contains n.Loc peers then " group=\"Peer\""
         else if Set.contains n.Loc onpaids then " group=\"OnPaid\""
         else if Set.contains n.Loc offpaids then " group=\"OffPaid\""
         else if Set.contains n.Loc borders then " group=\"Border\""
         else " group=\"Core\""
      
      let vars = 
         if pfxMap.ContainsKey n then sprintf " vars=\"prefix=%s\"" (string pfxMap.[n])
         else ""
      
      bprintf sb "  <node internal=\"%s\" asn=\"%s\" name=\"%s\"%s%s></node>\n" intern (string asn) 
         n.Loc group vars
   for (x, y) in Topology.edges topo do
      bprintf sb "  <edge source=\"%s\" target=\"%s\"></edge>\n" x.Loc y.Loc
   bprintf sb "%s" (coreAbstract n)
   bprintf sb "</topology>\n"
   string sb

let datacenter outDir k = 
   let (topo, pfxMap, tierMap) = Topology.Examples.fatTree k
   // concrete topology
   let cFilePol = sprintf "%s%sfat%d_con.pro" outDir File.sep k
   let cPol = writeDcPolConcrete (topo, pfxMap, tierMap)
   let cFileTopo = sprintf "%s%sfat%d_con.xml" outDir File.sep k
   let cTopo = writeTopologyConcrete topo
   System.IO.File.WriteAllText(cFilePol, cPol)
   System.IO.File.WriteAllText(cFileTopo, cTopo)
   // abstract topology
   let aFilePol = sprintf "%s%sfat%d_abs.pro" outDir File.sep k
   let aPol = writeDcPolAbstract()
   let aFileTopo = sprintf "%s%sfat%d_abs.xml" outDir File.sep k
   let aTopo = writeDcTopoAbstract (topo, pfxMap, tierMap) k
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo)

(* let backbone outDir n = 
   let topo = Topology.Examples.complete n
   // concrete topology
   let cFilePol = sprintf "%s%sbackbone%d_con.pro" outDir File.sep n
   let cPol = writeBackbonePolConcrete topo
   let cFileTopo = sprintf "%s%sbackbone%d_con.xml" outDir File.sep n
   let cTopo = writeTopologyConcrete topo
   System.IO.File.WriteAllText(cFilePol, cPol)
   System.IO.File.WriteAllText(cFileTopo, cTopo)
   // abstract topology
   let aFilePol = sprintf "%s%sbackbone%d_abs.pro" outDir File.sep n
   let aPol = writeBackbonePolAbstract topo
   let aFileTopo = sprintf "%s%sbackbone%d_abs.xml" outDir File.sep n
   let aTopo = writeBackboneTopoAbstract topo n
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo) *)

let core outDir n = 
   let topo, prefixes = Topology.Examples.core n
   // concrete topology
   let cFilePol = sprintf "%s%score%d_con.pro" outDir File.sep n
   let cPol = writeCorePolConcrete (topo, prefixes)
   let cFileTopo = sprintf "%s%score%d_con.xml" outDir File.sep n
   let cTopo = writeTopologyConcrete topo
   System.IO.File.WriteAllText(cFilePol, cPol)
   System.IO.File.WriteAllText(cFileTopo, cTopo)
   // abstract topology
   let aFilePol = sprintf "%s%score%d_abs.pro" outDir File.sep n
   let aPol = writeCorePolAbstract topo
   let aFileTopo = sprintf "%s%score%d_abs.xml" outDir File.sep n
   let aTopo = writeCoreTopoAbstract (topo, prefixes) n
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo)

let generate() = 
   let dir = "benchmarks"
   File.createDir dir
   for k in 4..2..24 do
      datacenter dir k
   for n in 10..10..240 do
      core dir n