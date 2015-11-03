module Consistency
open Extension.Error
open CGraph


/// An explanation for why a policy is unimplementable with BGP
type CounterExample = 
    | PrefViolation of CgState * CgState
    | TopoViolation of CgState * CgState

/// Preference ranking for each router based on possible routes
type Preferences = (CgState * Set<int>) list

/// Preferences for each internal router
type Ordering = 
    Map<string, Preferences>

/// Checks if the BGP routers can make local decisions not knowing about failures
/// based solely on the possibility to satisfy different preferences
val findOrdering: T -> Result<Ordering, CounterExample>

/// Conservative check that no failure scenario will result a violation of the policy
/// Checks that each more preferred node has a superset of paths of a less preferred node
val checkFailures: T -> Ordering -> Result<unit, CounterExample>

/// Precise check that no failure scenario will result in unspecified routes
/// by enumerating, and checking, all possible failure combinations up to some depth
val checkFailuresByEnumerating: int -> Topology.T -> T -> Ordering -> Result<unit, CounterExample>