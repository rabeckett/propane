module Regex

/// Extended regular expressions with negation, 
/// intersection, and character classes
type T

/// Reverse a regular expression
val rev: T -> T

/// Build a DFA for a regular expression directly using regular 
/// expression derivatives. Works well with complement,
/// intersection, and character classes. Produces near-minimal DFAs
type Automaton =
    {q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

/// Representation for a regex that we haven't built yet. 
/// Since we don't have the complete alphabet until we have built the entire
/// regular expression (due to partial AS topology information), we delay
/// the construction until the Build method is called in the builder object below.
type LazyT

/// Check if a regular expression denotes only single characters and 
/// if so, returns the set of characters it denotes
val singleLocations: Set<string> -> LazyT -> Set<string> option

/// Parameterize regular expression by an alphabet. Since f# does
/// not support ML-style functors, different objects can use different
/// alphabets. Client code must ensure a single object is used.
type REBuilder  = 
    new: Topology.T -> REBuilder
    member Build: LazyT -> T
    member Inside: LazyT
    member Outside: LazyT
    member Empty: LazyT
    member Epsilon: LazyT
    member Loc: string -> LazyT
    member Concat: LazyT list -> LazyT
    member Inter: LazyT list -> LazyT
    member Union: LazyT list -> LazyT
    member Negate: LazyT -> LazyT
    member Star: LazyT -> LazyT
    member MakeDFA: T -> Automaton
    member StartingLocs: T -> Set<string>
    (* Constraint-based builders *)
    member Path: string list -> LazyT
    member Internal: unit -> LazyT
    member External: unit -> LazyT
    member Any: unit -> LazyT
    member Waypoint: string -> LazyT
    member WaypointAny: string list -> LazyT
    member Avoid: string -> LazyT 
    member AvoidAny: string list -> LazyT
    member EndsAt: string -> LazyT
    member EndsAtAny: string list -> LazyT
    member StartsAt: string -> LazyT
    member StartsAtAny: string list -> LazyT
    member ValleyFree: seq<string list> -> LazyT