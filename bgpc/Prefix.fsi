module Prefix

/// A 32-bit prefix
type T

/// Range-based representation of a prefix
/// Ranges are kept sorted and overlapping
/// ranges are merged whenever possible
type Pred

/// Constructor for a prefix
val prefix: (uint32*uint32*uint32*uint32) -> uint32 -> T

/// Logical disjunction of two prefixes
val disj: Pred -> Pred -> Pred

/// Logical conjunction of two prefixes
val conj: Pred -> Pred -> Pred

/// Logical false
val bot: Pred

/// Logical true
val top: Pred

/// Logical Negation of a prefix
val negation: Pred -> Pred

/// Convert list of prefixes to a range-based representation
val toPredicate: T list -> Pred

/// Convert a range-based representation to a list of prefixes
val toPrefixes: Pred -> T list

