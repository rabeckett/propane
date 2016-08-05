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

let quagga (rc : RouterConfiguration) : string = 
  let settings = Args.getSettings()
  let sb = System.Text.StringBuilder()
  bprintf sb "router bgp %s\n" (getRouterName rc)
  // owned networks
  for n in rc.Networks do
    bprintf sb "  network %s\n" n
  // peer networks
  for pc in rc.PeerConfigurations do
    bprintf sb "  neighbor %s remote-as %s\n" (getPeerIp rc pc) pc.Peer
  for pc in rc.PeerConfigurations do
    match pc.InFilter with
    | None -> ()
    | Some f -> bprintf sb "  neighbor %s route-map %s in\n" (getPeerIp rc pc) f
    match pc.OutFilter with
    | None -> ()
    | Some f -> bprintf sb "  neighbor %s route-map %s out\n" (getPeerIp rc pc) f
  bprintf sb "!\n"
  // prefix lists
  for pl in rc.PrefixLists do
    bprintf sb "ip prefix-list %s %s %s\n" pl.Name (stringOfKind pl.Kind) pl.Prefix
  bprintf sb "!\n"
  // community lists
  for cl in rc.CommunityLists do
    bprintf sb "ip community-list standard %s %s " cl.Name (stringOfKind cl.Kind)
    for c in cl.Values do
      bprintf sb "%s " c
    bprintf sb "\n"
  bprintf sb "!\n"
  // as path lists
  for al in rc.AsPathLists do
    bprintf sb "ip as-path access-list %s %s %s\n" al.Name (stringOfKind al.Kind) al.Regex // name should be a number
  bprintf sb "!\n"
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
  // Create each router configuration, specialized by type
  for kv in nc.RouterConfigurations do
    let name = kv.Key
    let rc = kv.Value
    let output = quagga rc
    File.writeFileWithExtension (configDir + File.sep + name) "cfg" output
// TODO: Create the GNS3 file