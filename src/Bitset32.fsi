module Bitset32

/// High performance and memory-efficient bitset for 
/// manipulating and storing preferences.
/// To keep the type of the bitset abstract without 
/// introducing overhead, it uses the F# units of measurement
/// to ensure the underlying integer representation is inaccessible

[<Measure>] 
type B
type T = int<B>

val empty : T
 
val inline contains: int -> T -> bool

val inline add: int -> T -> T

val inline remove: int -> T -> T

val inline singleton: int -> T

val inline union: T -> T -> T

val inline intersect: T -> T -> T

val inline negate: T -> T

val inline difference: T -> T -> T

val inline isEmpty: T -> bool

val inline count: T -> int

val inline minimum: T -> int option

val inline iter: (int -> unit) -> T -> unit

val inline fold: ('a -> int -> 'a) -> 'a -> T -> 'a

val inline filter: (int -> bool) -> T -> T

val inline exists: (int -> bool) -> T -> bool

val inline forall: (int -> bool) -> T -> bool
