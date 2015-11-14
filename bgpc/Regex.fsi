module Regex

/// Extended regular expressions with negation, 
/// intersection, and character classes
type T

/// Check if a regular expression denotes only single characters and 
/// if so, returns the set of characters it denotes
val singleLocations: Set<string> -> T -> Set<string> option

/// Standard deterministic finite automaton, implemented 
/// using maps and sets for simplicity
type Automaton =
    {q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

/// Parameterize regular expression by an alphabet. Since f# does 
/// not support ML-style functors, different objects can use different 
/// alphabets. Client code must ensure a single builder object is used
type REBuilder  = 
    new: Topology.T -> REBuilder
    member Inside: T
    member Outside: T
    member Rev: (T -> T)
    member Empty: T
    member Epsilon: T
    member Loc: (string -> T)
    member Locs: (Set<string> -> T)
    member Concat: (T -> T -> T)
    member Inter: (T -> T -> T)
    member Union: (T -> T -> T)
    member Negate: (T -> T)
    member Star: (T -> T)
    member MakeDFA: (T -> Automaton)