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


/// Prefix representation of the form x1.x2.x3.x4/len ge (ge) le (le)
/// Prefix with an exact match will have negative ge, and le values
type Prefix = class
    val X1: int
    val X2: int
    val X3: int
    val X4: int
    val Len: int 
    val Ge: int
    val Le: int
    new(x1,x2,x3,x4,len,ge,le) = 
        {X1 = x1; X2 = x2; X3 = x3; X4 = x4; Len = len; Ge = ge; Le = le}
end


/// Community value representation as human-readable
/// pair of integers x:y (e.g., 100:2)
(* [<Struct>]
type Community = struct
    val X: int
    val Y: int
    new(x,y) = {X = x; Y = y}
end *)


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
type Community = class
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
type LocalPref = class
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
    val SetLocalPref: LocalPref
    val SetCommunity: List<Community>
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
    val InFilter: string // route map name
    val OutFilter: string
    new(p,i,o) = {Peer = p; InFilter = i; OutFilter = o}
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

let writeRouteMap sb (rm: RouteMap) = 
    failwith ""

  
let output(rc: RouterConfiguration) : string = 
    let sb = System.Text.StringBuilder()

    bprintf sb "router bgp %s\n" rc.Name

    // owned networks
    for n in rc.Networks do 
        bprintf sb "  network %s\n" n

    // peer networks
    for pc in rc.PeerConfigurations do 
        bprintf sb "  neighbor [ip] remote-as %s\n" pc.Peer
    for pc in rc.PeerConfigurations do 
        bprintf sb "  neighbor [ip] route-map %s in\n" pc.InFilter
        bprintf sb "  neighbor [ip] route-map %s out\n" pc.OutFilter
    
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
       

let generate(nc: NetworkConfiguration, outDir: string) =
    let sep = string System.IO.Path.DirectorySeparatorChar
    for kv in nc.RouterConfigurations do 
        let file = outDir + sep + "as" + kv.Key + ".quagga"
        let str = output kv.Value 
        System.IO.File.WriteAllText(file, str)
