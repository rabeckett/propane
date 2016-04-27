module Config

open Core.Printf
open System
open System.Collections.Generic


/// Kind of various configuration commands. 
/// Either permit or deny the prefix/ip/route/...
type Kind =
    | Permit = 0
    | Deny = 1


/// Direction of a filter list
type Direction =
    | In = 0
    | Out = 1


/// BGP Prefix list, consists of 
/// Name: prefix list name
/// Kind: either permit or deny routes
/// Prefix: The actual prefix, length, le, ge match
type PrefixList = class
    val Name: string
    val Kind: Kind
    val Prefix: string //Prefix
    new(k,n,p) = {Kind = k; Name = n; Prefix = p}
end

 
/// AS Path list, consists of 
/// Name:   as-path list name
/// Kind:   either permit or deny routes
/// Regex:  The path regex to match
type AsPathList = class
    val Name: string
    val Kind: Kind
    val Regex: string 
    new(k,n,r) = {Kind = k; Name = n; Regex = r}
end


/// Community list consists of 
/// Name:   community list name
/// Kind:   either permit or deny routes
/// Values: Community values to match (disjunction)
type CommunityList = class
    val Name: string
    val Kind: Kind
    val Values: List<string> // List<Community>
    new(k,n,vs) = {Kind = k; Name = n; Values = vs}
end


/// Simple wrapper around the new community value.
/// Represent no update with the null value
[<AllowNullLiteral>]
type SetCommunity = class
    val Value: string
    new(i) = {Value = i}
end


/// Simple wrapper around the community list to remove communities.
/// A null value indicates that no such list applies.
[<AllowNullLiteral>]
type DeleteCommunityList = class
    val Value: string
    new(i) = {Value = i}
end


/// Simple wrapper around the local preference value.
/// Represent the default value (100) with null.
[<AllowNullLiteral>]
type SetLocalPref = class
    val Value: int
    new(i) = {Value = i}
end


/// A policy list for (reusable) matching on several BGP attributes
/// Route maps will match using a policy list, and then perform updates.
/// Some vendors do not support policy lists, so each route-map will
/// need to inline the policy list at each usage site.
type PolicyList = class
    val Name: string
    val PrefixLists: List<string>
    val AsPathLists: List<string>
    val CommunityLists: List<string>
    new(n, pls, als, cls) = 
        {Name = n;
         PrefixLists = pls;
         AsPathLists = als;
         CommunityLists = cls;}
end


/// A route map, which matches using various filter lists.
/// Filter lists (prefix, as-path, community,...) are usually in conjunction.
type RouteMap = class
    val Name: string
    val Priority: int
    val PolicyList: string
    val SetLocalPref: SetLocalPref
    val SetCommunity: List<SetCommunity>
    val DeleteCommunity: DeleteCommunityList
    new(n,i,pl,slp,sc,dc) = 
        {Name = n; 
         PolicyList = pl;
         Priority = i; 
         SetLocalPref = slp;
         SetCommunity = sc;
         DeleteCommunity = dc}
end


/// A peer configuration represented by a pair of incoming and outgoing 
/// filters to that peer. Filters are represented using route maps only.
type PeerConfig = class
    val Peer: string
    val PeerIp: string
    val SourceIp: string
    val InFilter: string option // route map name
    val OutFilter: string option
    new(p,sip,pip,i,o) = {Peer = p; SourceIp = sip; PeerIp = pip; InFilter = i; OutFilter = o}
end


/// BGP router configuration contains a group of peer configurations.
/// If there is no network, then the string will be empty.
type RouterConfiguration = class
    val Name: string
    val Networks: List<string>
    val PrefixLists: List<PrefixList>
    val AsPathLists: List<AsPathList>
    val CommunityLists: List<CommunityList>
    val PolicyLists: List<PolicyList>
    val RouteMaps: List<RouteMap>
    val PeerConfigurations: List<PeerConfig>
    new(name, nwrk, pls, als, cls, pols, rms, pcs) = 
        {Name = name; 
         Networks=nwrk; 
         PrefixLists = pls;
         AsPathLists = als;
         CommunityLists = cls;
         PolicyLists = pols;
         RouteMaps = rms;
         PeerConfigurations = pcs}
end


/// Network-wide configuration as a collection of router configurations
type NetworkConfiguration = class
    val RouterConfigurations: Dictionary<string, RouterConfiguration>
    new(rcs) = {RouterConfigurations = rcs}
end


module Quagga =

  let stringOfKind(k:Kind) = 
      if k = Kind.Permit then "permit" else "deny"

  let lookupPolicyList (name: string) (pols: List<PolicyList>) = 
      pols |> Seq.find (fun p -> p.Name = name)

  let writePolList sb (pol: PolicyList) = 
      for cname in pol.CommunityLists do
          bprintf sb "  match community %s\n" cname
      for pname in pol.PrefixLists do 
          bprintf sb "  match ip address prefix-list %s\n" pname
      for aname in pol.AsPathLists do
          bprintf sb "  match as-path %s\n" aname

  let output sb (rc: RouterConfiguration) : string = 
      bprintf sb "router bgp %s\n" rc.Name

      // owned networks
      for n in rc.Networks do 
          bprintf sb "  network %s\n" n

      // peer networks
      for pc in rc.PeerConfigurations do 
          bprintf sb "  neighbor %s remote-as %s\n" pc.PeerIp pc.Peer
      for pc in rc.PeerConfigurations do 
          match pc.InFilter with 
          | None -> () 
          | Some f -> bprintf sb "  neighbor %s route-map %s in\n" pc.PeerIp f
          match pc.OutFilter with 
          | None -> () 
          | Some f -> bprintf sb "  neighbor %s route-map %s out\n" pc.PeerIp f
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
          bprintf sb "ip as-path access-list %s %s %s\n" al.Name (stringOfKind al.Kind) al.Regex  // name should be a number
      bprintf sb "!\n"

      // route maps
      for rm in rc.RouteMaps do 
          bprintf sb "route-map %s permit %d\n" rm.Name rm.Priority
          let pol = lookupPolicyList rm.PolicyList rc.PolicyLists
          writePolList sb pol
          let lp = rm.SetLocalPref
          if lp <> null then 
              bprintf sb "  set local-preference %d\n" lp.Value
          let dc = rm.DeleteCommunity 
          if dc <> null then
              bprintf sb "  set comm-list %s delete\n" dc.Value
          for c in rm.SetCommunity do  
              bprintf sb "  set community additive %s\n" c.Value
          bprintf sb "!\n"

      string sb


module Template = 

  let output sb (rc: RouterConfiguration) : string = 
      bprintf sb "router bgp %s.$router$\n" rc.Name

      // owned networks
      for n in rc.Networks do 
          bprintf sb "  network %s\n" n

      // peer networks
      for pc in rc.PeerConfigurations do 
          bprintf sb "  neighbor %s.%s.$peerIP$ remote-as %s\n" rc.Name pc.Peer pc.Peer 
      for pc in rc.PeerConfigurations do 
          match pc.InFilter with 
          | None -> () 
          | Some f -> bprintf sb "  neighbor %s.%s.$peerIP$ route-map %s in\n" rc.Name pc.Peer f
          match pc.OutFilter with 
          | None -> () 
          | Some f -> bprintf sb "  neighbor %s.%s.$peerIP$ route-map %s out\n" rc.Name pc.Peer f
      bprintf sb "!\n"

      // prefix lists
      for pl in rc.PrefixLists do 
          bprintf sb "ip prefix-list %s %s %s\n" pl.Name (Quagga.stringOfKind pl.Kind) pl.Prefix
      bprintf sb "!\n"

      // community lists
      for cl in rc.CommunityLists do 
          bprintf sb "ip community-list standard %s %s " cl.Name (Quagga.stringOfKind cl.Kind)
          for c in cl.Values do
              bprintf sb "%s " c
          bprintf sb "\n"
      bprintf sb "!\n"

      // as path lists
      for al in rc.AsPathLists do 
          bprintf sb "ip as-path access-list %s %s %s\n" al.Name (Quagga.stringOfKind al.Kind) al.Regex  // name should be a number
      bprintf sb "!\n"

      // route maps
      for rm in rc.RouteMaps do 
          bprintf sb "route-map %s permit %d\n" rm.Name rm.Priority
          let pol = Quagga.lookupPolicyList rm.PolicyList rc.PolicyLists
          Quagga.writePolList sb pol
          let lp = rm.SetLocalPref
          if lp <> null then 
              bprintf sb "  set local-preference %d\n" lp.Value
          let dc = rm.DeleteCommunity 
          if dc <> null then
              bprintf sb "  set comm-list %s delete\n" dc.Value
          for c in rm.SetCommunity do  
              bprintf sb "  set community additive %s\n" c.Value
          bprintf sb "!\n"

      string sb


let generate(nc: NetworkConfiguration, outDir: string) =
    let settings = Args.getSettings ()
    let sep = string System.IO.Path.DirectorySeparatorChar
    for kv in nc.RouterConfigurations do 
        if settings.IsAbstract then 
            let file = outDir + sep + kv.Key + ".template"
            let sb = System.Text.StringBuilder()
            let str = Template.output sb kv.Value 
            System.IO.File.WriteAllText(file, str)
        else
            let file = outDir + sep + kv.Key + ".quagga"
            let sb = System.Text.StringBuilder()
            let str = Quagga.output sb kv.Value 
            System.IO.File.WriteAllText(file, str)