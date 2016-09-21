module Config

open System
open System.Collections.Generic

let LOCAL_DELETE_COMMUNITY = "LOCAL"

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
type PrefixList = 
   class
      val Name : string
      val Kind : Kind
      val Prefix : string
      new(k, n, p) = 
         { Kind = k
           Name = n
           Prefix = p }
   end

/// AS Path list, consists of 
/// Name:   as-path list name
/// Kind:   either permit or deny routes
/// Regex:  A list of regex of the form deny, deny, ..., allow
type AsPathList = 
   class
      val Name : string
      val Regex : List<string>
      new(n, rs) = 
         { Name = n
           Regex = rs }
   end

/// Community list consists of 
/// Name:   community list name
/// Kind:   either permit or deny routes
/// Values: Community values to match (disjunction)
type CommunityList = 
   class
      val Name : string
      val Kind : Kind
      val Values : List<string> // List<Community>
      new(k, n, vs) = 
         { Kind = k
           Name = n
           Values = vs }
   end

/// Simple wrapper around the new community value.
type SetCommunity = 
   class
      val Value : string
      new(i) = { Value = i }
   end

/// Simple wrapper around the community to delete.
type DeleteCommunity = 
   class
      val Value : string
      new(i) = { Value = i }
   end

/// Simple wrapper around the local preference value.
/// Represent the default value (100) with null.
[<AllowNullLiteral>]
type SetLocalPref = 
   class
      val Value : int
      new(i) = { Value = i }
   end

/// Simple wrapper around the Multi-Exit-Discriminator value.
/// Represent the default value (80) with null.
[<AllowNullLiteral>]
type SetMED = 
   class
      val Value : int
      new(i) = { Value = i }
   end

/// Simple wrapper for path prepending.
/// Represent the default value (no prepending) with null.
[<AllowNullLiteral>]
type SetPathPrepend = 
   class
      val Value : int
      new(i) = { Value = i }
   end

/// A policy list for (reusable) matching on several BGP attributes
/// Route maps will match using a policy list, and then perform updates.
/// Some vendors do not support policy lists, so each route-map will
/// need to inline the policy list at each usage site.
type PolicyList = 
   class
      val Name : string
      val PrefixLists : List<string>
      val AsPathLists : List<string>
      val CommunityLists : List<string>
      new(n, pls, als, cls) = 
         { Name = n
           PrefixLists = pls
           AsPathLists = als
           CommunityLists = cls }
   end

/// A route map, which matches using various filter lists.
/// Filter lists (prefix, as-path, community,...) are usually in conjunction.
type RouteMap = 
   class
      val Name : string
      val Priority : int
      val PolicyList : string
      val SetLocalPref : SetLocalPref
      val SetMED : SetMED
      val SetPathPrepend : SetPathPrepend
      val mutable SetCommunities : List<SetCommunity>
      val DeleteCommunities : List<DeleteCommunity>
      new(n, i, pl, slp, smed, spre, sc, dcs) = 
         { Name = n
           PolicyList = pl
           Priority = i
           SetLocalPref = slp
           SetMED = smed
           SetPathPrepend = spre
           SetCommunities = sc
           DeleteCommunities = dcs }
   end

/// A peer configuration represented by a pair of incoming and outgoing 
/// filters to that peer. Filters are represented using route maps only.
type PeerConfig = 
   class
      val Peer : string
      val PeerAsn : string
      val PeerIp : string
      val SourceIp : string
      val mutable InFilter : string option // route map name
      val mutable OutFilter : string option
      new(p, pasn, sip, pip, i, o) = 
         { Peer = p
           PeerAsn = pasn
           SourceIp = sip
           PeerIp = pip
           InFilter = i
           OutFilter = o }
   end

/// BGP router configuration contains a group of peer configurations.
/// If there is no network, then the string will be empty.
type RouterConfiguration = 
   class
      val Name : string
      val NetworkAsn : string
      val RouterAsn : string
      val RouterID : int
      val Networks : List<Route.TempPrefix>
      val Aggregates : List<string>
      val PrefixLists : List<PrefixList>
      val AsPathLists : List<AsPathList>
      val CommunityLists : List<CommunityList>
      val PolicyLists : List<PolicyList>
      val mutable RouteMaps : List<RouteMap>
      val PeerConfigurations : List<PeerConfig>
      new(name, nasn, rasn, rid, nwrk, aggs, pls, als, cls, pols, rms, pcs) = 
         { Name = name
           NetworkAsn = nasn
           RouterAsn = rasn
           RouterID = rid
           Networks = nwrk
           Aggregates = aggs
           PrefixLists = pls
           AsPathLists = als
           CommunityLists = cls
           PolicyLists = pols
           RouteMaps = rms
           PeerConfigurations = pcs }
   end

/// Network-wide configuration as a collection of router configurations
type NetworkConfiguration = 
   class
      val NetworkAsn : string
      val RouterConfigurations : Dictionary<string, RouterConfiguration>
      new(rcs, nasn) = 
         { RouterConfigurations = rcs
           NetworkAsn = nasn }
   end

/// Remove a peer route map if the route map is never used (i.e., does not exist).
/// This is equivalent to having an explicit [permit any] route map.
let private deleteMissingRouteMaps (rc : RouterConfiguration) = 
   for pc in rc.PeerConfigurations do
      match pc.InFilter with
      | None -> ()
      | Some f -> 
         if not <| rc.RouteMaps.Exists(fun rm -> rm.Name = f) then pc.InFilter <- None
      match pc.OutFilter with
      | None -> ()
      | Some f -> 
         if not <| rc.RouteMaps.Exists(fun rm -> rm.Name = f) then pc.OutFilter <- None

/// Remove definition of as-path, community, prefix lists
/// that are never referenced in a valid route map
let private deleteUnusedLists (rc : RouterConfiguration) = 
   let mutable asLists = Set.empty
   let mutable commLists = Set.empty
   let mutable prefixLists = Set.empty
   for rm in rc.RouteMaps do
      let polList = rc.PolicyLists.Find(fun pl -> pl.Name = rm.PolicyList)
      asLists <- Set.union asLists (Set.ofSeq polList.AsPathLists)
      commLists <- Set.union commLists (Set.ofSeq polList.CommunityLists)
      prefixLists <- Set.union prefixLists (Set.ofSeq polList.PrefixLists)
   rc.AsPathLists.RemoveAll(fun al -> not <| asLists.Contains(al.Name)) |> ignore
   rc.CommunityLists.RemoveAll
      (fun cl -> cl.Name <> LOCAL_DELETE_COMMUNITY && not <| commLists.Contains(cl.Name)) |> ignore
   rc.PrefixLists.RemoveAll(fun pl -> not <| prefixLists.Contains(pl.Name))

/// Remove communities tagged to distinguish export actions, when
/// the community is never needed -- i.e., the actions is already unambiguous.
///
/// During generation unambigous exports will avoid matching on the community
/// and will not delete the community, but it will still get added from imports.
/// This function removes adding the community during imports when it is never used.
let private deleteExportComms (rc : RouterConfiguration) = 
   // collect all used delete communities
   let mutable deleted = Set.empty
   for rm in rc.RouteMaps do
      for d in rm.DeleteCommunities do
         let clname = d.Value
         let cllist = rc.CommunityLists.Find(fun cl -> cl.Name = clname)
         deleted <- Set.union deleted (Set.ofSeq cllist.Values)
   // remove unused
   for rm in rc.RouteMaps do
      rm.SetCommunities.RemoveAll(fun c -> c.Value.[0] = '1' && not (deleted.Contains(c.Value))) 
      |> ignore

/// Remove unnecessary configuration route maps, 
/// filters etc to simplify the configuration.
let clean (nc : NetworkConfiguration) = 
   for kv in nc.RouterConfigurations do
      let rc = kv.Value
      ()
//deleteUnusedLists rc |> ignore
//deleteExportComms rc |> ignore
//deleteMissingRouteMaps rc |> ignore