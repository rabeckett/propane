module Community

/// Community predicate represented as a disjunction
/// of known contained and not contained values
type Pred =
    | Bot
    | Val of Set<Set<string> * Set<string>>

/// False value
val bot: Pred 

/// True value
val top: Pred

/// A predicate testing for the existence of a community value
val value: string -> Pred

/// Logical disjunction of community predicates
val disj: Pred -> Pred -> Pred

/// Logical conjunction of community predicates
val conj: Pred -> Pred -> Pred

/// Logical negation of a community predicate
val negate: Pred -> Pred