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

let getRouterAsn (rc : RouterConfiguration) = 
  let s = Args.getSettings()
  if s.IsAbstract then rc.Name + ".$router$"
  else rc.RouterAsn

let getPeerIp (rc : RouterConfiguration) (pc : PeerConfig) = 
  let s = Args.getSettings()
  if s.IsAbstract then sprintf "%s.%s.$peerIP$" rc.Name pc.Peer
  else pc.PeerIp

let getPeerAsn rInternal (rc : RouterConfiguration) (pc : PeerConfig) = 
  if Set.contains rc.Name rInternal then pc.PeerAsn
  else rc.NetworkAsn

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

let confedAsn rInternal (pc : PeerConfig) = 
  if Set.contains pc.Peer rInternal then Some pc.PeerAsn
  else None

let quagga (rInternal : Set<string>) (rc : RouterConfiguration) : string = 
  let settings = Args.getSettings()
  let sb = System.Text.StringBuilder()
  // generate interface info
  bprintf sb "%s" (quaggaInterfaces rc)
  // bgp network and peers
  bprintf sb "router bgp %s\n" (getRouterAsn rc)
  bprintf sb "  no synchronization\n"
  // convert router id to prefix
  let (_, b, c, d) = Route.Bitwise.toDotted rc.RouterID
  bprintf sb "  bgp router-id 192.%d.%d.%d\n" b c d
  // Ensure private ASNs don't appear by using BGP confederations
  if rInternal.Contains rc.Name then 
    if rc.NetworkAsn <> rc.RouterAsn then 
      bprintf sb "  bgp confederation identifier %s\n" rc.NetworkAsn
      let allConfedAsns = 
        rc.PeerConfigurations
        |> Seq.choose (confedAsn rInternal)
        |> List.ofSeq
        |> Util.List.joinBy " "
      bprintf sb "  bgp confederation peers %s\n" allConfedAsns
  for n in rc.Networks do
    bprintf sb "  network %s\n" (string n)
  // generate aggregates
  for aggPfx in rc.Aggregates do
    bprintf sb "  aggregate-address %s as-set summary-only\n" aggPfx
  for pc in rc.PeerConfigurations do
    bprintf sb "  neighbor %s remote-as %s\n" (getPeerIp rc pc) (getPeerAsn rInternal rc pc)
    if rInternal.Contains(pc.Peer) then 
      let peerIp = getPeerIp rc pc
      bprintf sb "  neighbor %s next-hop-self\n" peerIp
      bprintf sb "  neighbor %s send-community both\n" peerIp
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
    for d in rm.DeleteCommunities do
      bprintf sb "  set comm-list %s delete\n" d.Value
    let mutable comms = ""
    for c in rm.SetCommunities do
      comms <- comms + c.Value + " "
    if comms <> "" then bprintf sb "  set community additive %s\n" comms
    if rm.SetMED <> null then bprintf sb "  set metric %d\n" rm.SetMED.Value
    if rm.SetPathPrepend <> null then 
      let asn = string rc.NetworkAsn
      let prepends = Util.Format.repeat asn rm.SetPathPrepend.Value
      bprintf sb "  set prepend %s\n" prepends
    bprintf sb "!\n"
  string sb

let core (rInternal : Set<string>) (nc : NetworkConfiguration) : string = 
  let sb = System.Text.StringBuilder()
  let mutable i = 1
  let nodeMap = Dictionary()
  let hostMap = Dictionary()
  // populate the node map [router name --> node id]
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    nodeMap.[name] <- i
    i <- i + 1
  // populate the host map [router name --> network, router id, host id]
  for kv in nc.RouterConfigurations do
    let rc = kv.Value
    if rc.Networks.Count > 0 then 
      let n = rc.Networks.[0]
      
      let (hSub, rSub) = 
        match n with
        | Route.ConcretePfx(a, b, c, d, slash) -> 
          if slash = 32 then (string n, string n)
          else 
            let x = sprintf "%d.%d.%d.%d/%d" a b c (d + 1) slash
            let y = sprintf "%d.%d.%d.%d/%d" a b c (d + 2) slash
            (x, y)
        | _ -> failwith "unreachable"
      hostMap.[rc.Name] <- (hSub, rSub, nodeMap.[rc.Name], i)
      i <- i + 1
  // generate router information
  let mutable i = 1
  for kv in nc.RouterConfigurations do
    let rc = kv.Value
    // per router
    bprintf sb "node n%d {\n" i
    bprintf sb "    type router\n"
    bprintf sb "    model router\n"
    bprintf sb "    network-config {\n"
    bprintf sb "\thostname %s\n" rc.Name
    bprintf sb "\t!\n"
    bprintf sb "%s\n" (quaggaInterfaces rc |> Util.Format.indent 1 true)
    // add host interface if needed
    if hostMap.ContainsKey(rc.Name) then 
      let (_, rSub, rId, hId) = hostMap.[rc.Name]
      bprintf sb "\tinterface eth%d\n" (rc.PeerConfigurations.Count)
      bprintf sb "\t ip address %s\n" rSub
      bprintf sb "\t!\n"
    bprintf sb "    }\n"
    bprintf sb "    iconcoords {100.0 100.0}\n"
    bprintf sb "    labelcoords {100.0 128.0}\n"
    let mutable j = 0
    // interfaces to other routers
    for peer in rc.PeerConfigurations do
      // TODO: external neighbors
      if nodeMap.ContainsKey(peer.Peer) then 
        bprintf sb "    interface-peer {eth%d n%d}\n" j nodeMap.[peer.Peer]
        j <- j + 1
    // interfaces to attached host
    if hostMap.ContainsKey(rc.Name) then 
      let (_, _, _, hId) = hostMap.[rc.Name]
      bprintf sb "    interface-peer {eth%d n%d}" j hId
      j <- j + 1
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
  // generate host information
  for kv in hostMap do
    let name = kv.Key
    let (hSub, _, rId, hId) = kv.Value
    bprintf sb "node n%d {\n" hId
    bprintf sb "    type router\n"
    bprintf sb "    model host\n"
    bprintf sb "    network-config {\n"
    bprintf sb "\thostname HOST_%s\n" name
    bprintf sb "\t!\n"
    bprintf sb "\tinterface eth0\n"
    bprintf sb "\t ip address %s\n" hSub
    bprintf sb "\t!\n"
    bprintf sb "    }\n"
    bprintf sb "    canvas c1\n"
    bprintf sb "    iconcoords {100.0 100.0}\n"
    bprintf sb "    labelcoords {100.0 128.0}\n"
    bprintf sb "    interface-peer {eth0 n%d}\n" rId
    bprintf sb "}\n\n"
  // Add links for hosts
  for kv in hostMap do
    let (_, _, rId, hId) = kv.Value
    bprintf sb "link l%d_%d {\n" rId hId
    bprintf sb "    nodes {n%d n%d}\n" rId hId
    bprintf sb "}\n\n"
  // Add links between routers
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
          bprintf sb "link l%d_%d {\n" x y
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

let addFakeExternalConfigs (nc : NetworkConfiguration) = 
  // collect all external peer connections
  let mutable peerMap = Map.empty
  let mutable allPeers = Set.empty
  let mutable maxID = 0
  // collect peer information 
  for pair in nc.RouterConfigurations do
    let name = pair.Key
    let rc = pair.Value
    maxID <- max rc.RouterID maxID
    allPeers <- Set.add name allPeers
    for pc in rc.PeerConfigurations do
      peerMap <- Util.Map.adjust (pc.Peer, pc.PeerAsn) Set.empty 
                   (Set.add (rc.Name, rc.RouterAsn, pc.SourceIp, pc.PeerIp)) peerMap
  // add fake external peers
  let i = ref maxID
  let j = ref 0
  Map.iter (fun (exPeer, exAsn) neighbors -> 
    if not (allPeers.Contains(exPeer)) then 
      incr i
      let nwrks = List()
      let pcs = List()
      nwrks.Add(Route.ConcretePfx(172, 0, 0, 0, 24))
      for (n, asn, srcIp, peerIp) in neighbors do
        let pc = PeerConfig(n, asn, peerIp, srcIp, None, None)
        pcs.Add(pc)
      let rc = 
        RouterConfiguration
          (exPeer, nc.NetworkAsn, exAsn, !i, nwrks, List(), List(), List(), List(), List(), List(), 
           pcs)
      nc.RouterConfigurations.[exPeer] <- rc) peerMap

let generate (res : Abgp.CompilationResult) = 
  let settings = Args.getSettings()
  let out = settings.OutDir
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
  addFakeExternalConfigs nc
  core rInternal nc |> File.writeFileWithExtension (out + File.sep + "core") "imn"
// Write test files