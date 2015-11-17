module Consistency
open Extension.Error
open CGraph


/// An explanation for why a policy is unimplementable with BGP
type CounterExample = CgState * CgState

/// Preference ranking for each router based on possible routes
type Preferences = seq<CgState>

/// Preferences for each internal router
type Ordering = Map<string, Preferences>

/// Checks if the BGP routers can make local decisions not knowing about failures
/// based solely on the possibility to satisfy different preferences
val findOrdering: T -> Result<Ordering, CounterExample>