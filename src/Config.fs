module Config


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
[<Struct>]
type Community = struct
    val X: int
    val Y: int
    new(x,y) = {X = x; Y = y}
end


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
    val Value: int
    new(i) = {Value = i}
end


/// Simple wrapper around the local preference value.
/// Represent the default value (100) with null.
[<AllowNullLiteral>]
type SetLocalPref = class
    val Value: int
    new(i) = {Value = i}
end


/// A route map, which matches using various filter lists.
/// Filter lists (prefix, as-path, community,...) are usually in conjunction.
type RouteMap = class
    val Name: String
    val Priority: int
    val PrefixLists: List<string>
    val AsPathLists: List<string>
    val CommunityLists: List<string>
    val SetCommunity: SetCommunity
    val SetLocalPref: SetLocalPref
    new(n,i,pls,als,cls,sc,slp) = 
        {Name = n; 
         Priority = i; 
         PrefixLists = pls;
         AsPathLists = als;
         CommunityLists = cls;
         SetCommunity = sc; 
         SetLocalPref = slp}
end


/// A peer configuration represented by a pair of incoming and outgoing 
/// filters to that peer. Filters are represented using route maps only.
type PeerConfig = class
    val Peer: string
    val InFilters: List<string> // route map name
    val OutFilters: List<string>
    new(p,ifs,ofs) = {Peer = p; InFilters = ifs; OutFilters = ofs}
end


/// BGP router configuration contains a group of peer configurations.
/// If there is no network, then the string will be empty.
type RouterConfiguration = class
    val Name: string
    val Network: string
    val RouteMaps: List<RouteMap>
    val PrefixLists: List<PrefixList>
    val AsPathLists: List<AsPathList>
    val CommunityLists: List<CommunityList>
    val PeerConfigurations: List<PeerConfig>
    new(name, nwrk, rms, pls, als, cls, pcs) = 
        {Name = name; 
         Network=nwrk; 
         RouteMaps = rms;
         PrefixLists = pls;
         AsPathLists = als;
         CommunityLists = cls;
         PeerConfigurations = pcs}
end


/// Network-wide configuration as a collection of router configurations
type NetworkConfiguration = class
    val RouterConfigurations: Dictionary<string, RouterConfiguration>
    new(rcs) = {RouterConfigurations = rcs}
end




open Core.Printf


let stringOfKind(k:Kind) = 
    if k = Kind.Permit then "permit" else "deny"

  
 
let output(rc: RouterConfiguration) : string = 
    let sb = System.Text.StringBuilder()

    bprintf sb "router bgp %s\n" rc.Name

    // peer networks
    for pc in rc.PeerConfigurations do 
        bprintf sb "  neighbor [ip] remote-as %s\n" pc.Peer
    for pc in rc.PeerConfigurations do 
        for f in pc.InFilters do 
            bprintf sb "  neighbor [ip] route-map %s in\n" f 
        for f in pc.OutFilters do 
            bprintf sb "  neighbor [ip] route-map %s out\n" f
    bprintf sb "!\n"

    // this network
    if rc.Network <> "" then
        bprintf sb "network %s\n" rc.Network

    // prefix lists
    for pl in rc.PrefixLists do 
        bprintf sb "ip prefix-list %s %s %s\n" pl.Name (stringOfKind pl.Kind) pl.Prefix
    bprintf sb "!\n"

    // community lists
    for cl in rc.CommunityLists do 
        bprintf sb "ip community-list standard %s %s " cl.Name (stringOfKind cl.Kind)
        for c in cl.Values do
            bprintf sb "100:%s " c
        bprintf sb "\n"
    bprintf sb "!\n"

    // as path lists
    for al in rc.AsPathLists do 
        bprintf sb "ip as-path access-list %s %s %s\n" al.Name (stringOfKind al.Kind) al.Regex  // name should be a number
    bprintf sb "!\n"

    // route maps
    for rm in rc.RouteMaps do 
        bprintf sb "route-map %s permit %d\n" rm.Name rm.Priority
        for cname in rm.CommunityLists do
            bprintf sb "  match community %s\n" cname
        for pname in rm.PrefixLists do 
            bprintf sb "  match ip address prefix-list %s\n" pname
        for aname in rm.AsPathLists do
            bprintf sb "  match as-path %s" aname
        let lp = rm.SetLocalPref
        if lp <> null then 
            bprintf sb "  set local-preference %d\n" lp.Value
        bprintf sb "!\n"

    string sb
       

let generate(nc: NetworkConfiguration, outDir: string) =
    let sep = string System.IO.Path.DirectorySeparatorChar
    for kv in nc.RouterConfigurations do 
        let file = outDir + sep + "as" + kv.Key + ".quagga"
        let str = output(kv.Value) 
        System.IO.File.WriteAllText(file, str)
