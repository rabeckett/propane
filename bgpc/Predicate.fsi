module Predicate

type Pair = 
    {Prefix: Prefix.Pred;
     Comm: Community.Pred}

type T =
    | Pred of Set<Pair>

/// False value
val bot: T

/// True value
val top: T

/// Prefix matches a.b.c.d/e
val prefix: (uint32 * uint32 * uint32 * uint32) -> uint32 -> T

/// Build directly from a prefix predicate
val prefixPred: Prefix.Pred -> T

/// Community matches a string
val community: string -> T

/// Build directly from a community predicate
val communityPred: Community.Pred -> T

/// Disjunction of two predicates
val disj: T -> T -> T

/// Conjunction of two predicates
val conj: T -> T -> T

/// Negation of a predicate
val negate: T -> T

/// All prefixes that appear in the predicate
val getPrefixes: T -> seq<Prefix.Pred>

/// Generate a simple, near-minimal, symbolic example that satisfies the predicate
val example: T -> string


/// Module for unit tests
module Test = 
    val run: unit -> unit