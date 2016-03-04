module Regex


/// Extended regular expressions with negation, 
/// intersection, and character classes
type T

/// Build a DFA for a regular expression directly using regular 
/// expression derivatives. Works well with complement,
/// intersection, and character classes. Produces near-minimal DFAs
type Automaton =
    {q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

/// Reverse a regular expression
val rev: T -> T

/// Smart constructors for regular expressions. These functions are exposed
/// for reuse with the DFA to automaton conversion.
/// To build regular expressions with a topology, use the REBuilder 
val empty: T
val epsilon: T
val loc: string -> T
val locs: Set<string> -> T
val star: T -> T
val negate: Set<string> -> T -> T
val concat: T -> T -> T
val concatAll: T list -> T
val inter: T -> T -> T
val interAll: T list -> T
val union: T -> T -> T
val unionAll: T list -> T

/// Check if an automaton denotes the empty set,
/// and if not, return an example sequence
val emptiness: Automaton -> (string list) option

/// Representation for a regex that we haven't built yet. 
/// Since we don't have the complete alphabet until we have built the entire
/// regular expression (due to partial AS topology information), we delay
/// the construction until the Build method is called in the builder object below.
type LazyT

/// Parameterize regular expression by an alphabet. Since f# does
/// not support ML-style functors, different objects can use different
/// alphabets. Client code must ensure a single object is used.
type REBuilder  = 
    new: Topology.T -> REBuilder
    member Alphabet: unit -> Set<string>
    member Topo: unit -> Topology.T
    member Build: Route.Predicate -> int -> LazyT -> T
    member Inside: LazyT
    member Outside: LazyT
    member Empty: LazyT
    member Epsilon: LazyT
    member Loc: string -> LazyT
    member Locs: string list -> LazyT
    member Concat: LazyT list -> LazyT
    member Inter: LazyT list -> LazyT
    member Union: LazyT list -> LazyT
    member Negate: LazyT -> LazyT
    member Star: LazyT -> LazyT
    (* *)
    member SingleLocations: LazyT -> Set<string> option
    member MakeDFA: T -> Automaton
    member StartingLocs: Automaton -> Set<string>
    (* Constraint-based builders *)
    member Path: string list -> LazyT
    member Internal: unit -> LazyT
    member External: unit -> LazyT
    member Any: unit -> LazyT
    member Always: string list -> LazyT
    member Through: string list -> LazyT
    member Avoid: string list -> LazyT
    member End: string list -> LazyT
    member Start: string list -> LazyT
    member ValleyFree: seq<string list> -> LazyT
    member Enter: string list -> LazyT
    member Exit: string list -> LazyT
    member Only: LazyT -> LazyT


/// Run unit tests
module Test = 
    val run: unit -> unit