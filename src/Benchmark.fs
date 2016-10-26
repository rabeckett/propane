module Benchmark

open Core.Printf
open System.Text
open Topology
open Util

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
let backboneAbstract = """
  <!-- Abstract Nodes -->
  <abstractnode internal="true" label="Inside"></abstractnode>
  <abstractnode internal="false" label="Cust"></abstractnode>
  <abstractnode internal="false" label="Peer"></abstractnode>
  <abstractnode internal="false" label="OnPaid"></abstractnode>
  <abstractnode internal="false" label="OffPaid"></abstractnode>
  <!-- Abstract Edges -->
  <abstractedge source="Inside" target="Cust" labels="(E1,E2)"></abstractedge>
  <abstractedge source="Inside" target="Peer" labels="(E3,E4)"></abstractedge>
  <abstractedge source="Inside" target="OnPaid" labels="(E5,E6)"></abstractedge>
  <abstractedge source="Inside" target="OffPaid" labels="(E7,E8)"></abstractedge>
  <abstractedge source="Inside" target="Inside" labels="(E9,E9)"></abstractedge>
"""
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

let writeDcPolConcrete ((topo, pfxMap, tierMap) : _ * Topology.Examples.Prefixes * Topology.Examples.Tiers) = 
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
      bprintf sb "  %s => end(%s),\n" (string prefix) loc
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
   bprintf sb "  bogon => drop\n,"
   bprintf sb "  private => drop,\n"
   bprintf sb "  T0.$prefix$ => end(T0),\n"
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

let getPeerGroups topo = 
   let mutable custs = Set.empty
   let mutable peers = Set.empty
   let mutable onpaids = Set.empty
   let mutable offpaids = Set.empty
   for v in Topology.vertices topo do
      let l = v.Loc
      if l.StartsWith("Cust") then custs <- Set.add l custs
      if l.StartsWith("Peer") then peers <- Set.add l peers
      if l.StartsWith("OnPaid") then onpaids <- Set.add l onpaids
      if l.StartsWith("OffPaid") then offpaids <- Set.add l offpaids
   (custs, peers, onpaids, offpaids)

let writeBackbonePolConcrete (topo : Topology.T) = 
   let (custs, peers, onpaids, offpaids) = getPeerGroups topo
   let sb = StringBuilder()
   bprintf sb "define Cust = %s\n" (Util.Set.toString custs)
   bprintf sb "define Peer = %s\n" (Util.Set.toString peers)
   bprintf sb "define OnPaid = %s\n" (Util.Set.toString onpaids)
   bprintf sb "define OffPaid = %s\n" (Util.Set.toString offpaids)
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not (transit(NonCust, NonCust))\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop\n,"
   bprintf sb "  private => drop,\n"
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid) & end(out)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n"
   string sb

let writeBackbonePolAbstract (topo : Topology.T) = 
   let sb = StringBuilder()
   bprintf sb "define NonCust = Peer + OnPaid + OffPaid\n"
   bprintf sb "%s" definitions
   bprintf sb "define notransit = {\n"
   bprintf sb "  true => not (transit(NonCust, NonCust))\n"
   bprintf sb "}\n\n"
   bprintf sb "define routing = {\n"
   bprintf sb "  bogon => drop\n,"
   bprintf sb "  private => drop,\n"
   bprintf sb "  true => exit(Cust >> Peer >> OnPaid >> OffPaid) & end(out)\n"
   bprintf sb "}\n\n"
   bprintf sb "define main = routing & notransit\n"
   string sb

let writeBackboneTopoAbstract (topo : Topology.T) = 
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
   bprintf sb "%s" backboneAbstract
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
   let aTopo = writeDcTopoAbstract (topo, pfxMap, tierMap)
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo)

let backbone outDir n = 
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
   let aTopo = writeBackboneTopoAbstract topo
   System.IO.File.WriteAllText(aFilePol, aPol)
   System.IO.File.WriteAllText(aFileTopo, aTopo)

let generate() = 
   let dir = "benchmarks"
   File.createDir dir
   for k in 4..2..30 do
      datacenter dir k
   for n in 10..10..200 do
      backbone dir n