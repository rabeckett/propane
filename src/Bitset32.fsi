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
val contains : int -> T -> bool
val add : int -> T -> T
val remove : int -> T -> T
val singleton : int -> T
val union : T -> T -> T
val intersect : T -> T -> T
val negate : T -> T
val difference : T -> T -> T
val isEmpty : T -> bool
val count : T -> int
val minimum : T -> int option
val iter : (int -> unit) -> T -> unit
val fold : ('a -> int -> 'a) -> 'a -> T -> 'a
val filter : (int -> bool) -> T -> T
val exists : (int -> bool) -> T -> bool
val forall : (int -> bool) -> T -> bool
val toSet : T -> Set<int>
