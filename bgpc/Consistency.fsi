module Consistency
open Extension.Error
open CGraph


/// An explanation for why a policy is unimplementable with BGP
type CounterExample = CgState * CgState

/// Preference ranking for each router based on possible routes
type Preferences = seq<CgState>

/// Preferences for each internal router
type Ordering = Map<string, Preferences>

/// Conservative check if the BGP routers can make local decisions not knowing about failures
val findOrderingConservative: (CGraph.T -> Result<Ordering, CounterExample>)

/// Exact check if BGP routes can make local decisions by enumerating failures
val findOrderingEnumerate: int -> (CGraph.T -> Result<Ordering, CounterExample>)