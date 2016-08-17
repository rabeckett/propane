module Generate

open Config
open Core.Printf
open System.Collections.Generic
open Util

let stringOfKind (k : Kind) = 
  if k = Kind.Permit then "permit"
  else "deny"

let lookupPolicyList (name : string) (pols : List<PolicyList>) = 
  pols |> Seq.find (fun p -> p.Name = name)

let writePolList sb (pol : PolicyList) = 
  for cname in pol.CommunityLists do
    bprintf sb "  match community %s\n" cname
  for pname in pol.PrefixLists do
    bprintf sb "  match ip address prefix-list %s\n" pname
  for aname in pol.AsPathLists do
    bprintf sb "  match as-path %s\n" aname

let getRouterName (rc : RouterConfiguration) = 
  let s = Args.getSettings()
  if s.IsAbstract then rc.Name + ".$router$"
  else rc.Name

let getPeerIp (rc : RouterConfiguration) (pc : PeerConfig) = 
  let s = Args.getSettings()
  if s.IsAbstract then sprintf "%s.%s.$peerIP$" rc.Name pc.Peer
  else pc.PeerIp

let getSourceIp (rc : RouterConfiguration) (pc : PeerConfig) = 
  let s = Args.getSettings()
  if s.IsAbstract then sprintf "%s.%s.$sourceIP$" rc.Name pc.Peer
  else pc.SourceIp

let quaggaInterfaces (rc : RouterConfiguration) : string = 
  let sb = System.Text.StringBuilder()
  let mutable i = 0
  for pc in rc.PeerConfigurations do
    bprintf sb "interface eth%d\n" i
    bprintf sb " ip address %s/24\n" (getSourceIp rc pc)
    bprintf sb "!\n"
    i <- i + 1
  string sb

let quagga (rInternal : Set<string>) (rc : RouterConfiguration) : string = 
  let settings = Args.getSettings()
  let sb = System.Text.StringBuilder()
  // generate interface info
  bprintf sb "%s" (quaggaInterfaces rc)
  // bgp network and peers
  bprintf sb "router bgp %s\n" (getRouterName rc)
  bprintf sb "  no synchronization\n"
  // convert router id to prefix
  let (_, b, c, d) = Route.Bitwise.toDotted rc.RouterID
  bprintf sb "  bgp router-id 192.%d.%d.%d\n" b c d
  for n in rc.Networks do
    bprintf sb "  network %s\n" n
  for pc in rc.PeerConfigurations do
    bprintf sb "  neighbor %s remote-as %s\n" (getPeerIp rc pc) pc.Peer
    if rInternal.Contains(pc.Peer) then 
      bprintf sb "  neighbor %s send-community both\n" (getPeerIp rc pc)
  for pc in rc.PeerConfigurations do
    match pc.InFilter with
    | None -> ()
    | Some f -> bprintf sb "  neighbor %s route-map %s in\n" (getPeerIp rc pc) f
    match pc.OutFilter with
    | None -> ()
    | Some f -> bprintf sb "  neighbor %s route-map %s out\n" (getPeerIp rc pc) f
  if rc.PeerConfigurations.Count > 0 then bprintf sb "!\n"
  // prefix lists
  for pl in rc.PrefixLists do
    bprintf sb "ip prefix-list %s %s %s\n" pl.Name (stringOfKind pl.Kind) pl.Prefix
  if rc.PrefixLists.Count > 0 then bprintf sb "!\n"
  // community lists
  for cl in rc.CommunityLists do
    bprintf sb "ip community-list standard %s %s " cl.Name (stringOfKind cl.Kind)
    for c in cl.Values do
      bprintf sb "%s " c
    bprintf sb "\n"
  if rc.CommunityLists.Count > 0 then bprintf sb "!\n"
  // as path lists
  for al in rc.AsPathLists do
    bprintf sb "ip as-path access-list %s %s %s\n" al.Name (stringOfKind al.Kind) al.Regex // name should be a number
  if rc.AsPathLists.Count > 0 then bprintf sb "!\n"
  // route maps
  for rm in rc.RouteMaps do
    bprintf sb "route-map %s permit %d\n" rm.Name rm.Priority
    let pol = lookupPolicyList rm.PolicyList rc.PolicyLists
    writePolList sb pol
    let lp = rm.SetLocalPref
    if lp <> null then bprintf sb "  set local-preference %d\n" lp.Value
    let dc = rm.DeleteCommunity
    if dc <> null then bprintf sb "  set comm-list %s delete\n" dc.Value
    for c in rm.SetCommunity do
      bprintf sb "  set community additive %s\n" c.Value
    bprintf sb "!\n"
  string sb

let core (rInternal : Set<string>) (nc : NetworkConfiguration) : string = 
  let sb = System.Text.StringBuilder()
  let mutable i = 1
  let nodeMap = Dictionary()
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    nodeMap.[name] <- i
    i <- i + 1
  let mutable i = 1
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    // per router
    bprintf sb "node n%d {\n" i
    bprintf sb "    type router\n"
    bprintf sb "    model router\n"
    bprintf sb "    network-config {\n"
    bprintf sb "\thostname AS%s\n" rc.Name
    bprintf sb "\t!\n"
    bprintf sb "%s\n" (quaggaInterfaces rc |> Util.Format.indent 1 true)
    bprintf sb "    }\n"
    bprintf sb "    iconcoords {100.0 100.0}\n"
    bprintf sb "    labelcoords {100.0 135.0}\n"
    let mutable j = 0
    for peer in rc.PeerConfigurations do
      // TODO: external neighbors
      if nodeMap.ContainsKey(peer.Peer) then 
        bprintf sb "    interface-peer {eth%d n%d}\n" j nodeMap.[peer.Peer]
        j <- j + 1 // TODO: do  this proper
    bprintf sb "    canvas c1\n"
    bprintf sb "    services {zebra BGP vtysh IPForward}\n"
    bprintf sb "    custom-config {\n"
    bprintf sb "\tcustom-config-id service:zebra:/usr/local/etc/quagga/Quagga.conf\n"
    bprintf sb "\tcustom-command /usr/local/etc/quagga/Quagga.conf\n"
    bprintf sb "\tconfig {\n"
    bprintf sb "%s" (quagga rInternal rc |> Util.Format.indent 1 true)
    bprintf sb "}\n"
    bprintf sb "    }\n"
    bprintf sb "    custom-config {\n"
    bprintf sb "\tcustom-config-id service:zebra\n"
    bprintf sb "\tcustom-command zebra\n"
    bprintf sb "\tconfig {\n"
    bprintf sb "\t('/usr/local/etc/quagga', '/var/run/quagga')\n"
    bprintf sb "\t('/usr/local/etc/quagga/Quagga.conf', 'quaggaboot.sh')\n"
    bprintf sb "\t35\n"
    bprintf sb "\t('sh quaggaboot.sh zebra',)\n"
    bprintf sb "\t('killall zebra',)\n"
    bprintf sb "\t}\n"
    bprintf sb "    }\n"
    bprintf sb "}\n\n"
    i <- i + 1
  // Add links
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    for peer in rc.PeerConfigurations do
      // TODO: external neighbors
      if nodeMap.ContainsKey(peer.Peer) then 
        let x = nodeMap.[name]
        let y = nodeMap.[peer.Peer]
        // only do one direction
        if x <= y then 
          bprintf sb "link l%d%d {\n" x y
          bprintf sb "    nodes {n%d n%d}\n" x y
          bprintf sb "}\n\n"
  bprintf sb "canvas c1 {\n"
  bprintf sb "    name {Canvas1}\n"
  bprintf sb "    size {900 706.0}\n"
  bprintf sb "}\n\n"
  bprintf sb "option global {\n"
  bprintf sb "    interface_names no\n"
  bprintf sb "    ip_addresses yes\n"
  bprintf sb "    ipv6_addresses no\n"
  bprintf sb "    node_labels yes\n"
  bprintf sb "    link_labels yes\n"
  bprintf sb "    ipsec_configs yes\n"
  bprintf sb "    remote_exec no\n"
  bprintf sb "    exec_errors yes\n"
  bprintf sb "    show_api no\n"
  bprintf sb "    background_images no\n"
  bprintf sb "    annotations yes\n"
  bprintf sb "    grid yes\n"
  bprintf sb "}\n"
  string sb

let internalRouters (nc : NetworkConfiguration) : Set<string> = 
  let mutable rin = Set.empty
  for kv in nc.RouterConfigurations do
    rin <- Set.add kv.Key rin
  rin

let generate (out : string) (res : Abgp.CompilationResult) = 
  let settings = Args.getSettings()
  // Create output directory
  File.createDir out
  // Generate intermediate representation
  File.writeFileWithExtension (out + File.sep + "configs") "ir" (Abgp.format res.Abgp)
  // Get the low-level configurations
  let nc : NetworkConfiguration = Abgp.toConfig res.Abgp
  let configDir = out + File.sep + "configs"
  File.createDir configDir
  let rInternal = internalRouters nc
  // Create each router configuration, specialized by type
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    let output = quagga rInternal rc
    output |> File.writeFileWithExtension (configDir + File.sep + name) "cfg"
  // Write CORE emulator save file
  core rInternal nc |> File.writeFileWithExtension (out + File.sep + "core") "imn"