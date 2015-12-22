module Prefix

[<StructuralEquality; StructuralComparison>]
type T = {X1: uint32; X2: uint32; X3: uint32; X4: uint32; Slash: uint32}
type Pred = (uint32 * uint32) list

val prefix: (uint32*uint32*uint32*uint32) -> uint32 -> T

val disj: Pred -> Pred -> Pred
val conj: Pred -> Pred -> Pred
val bot: Pred
val top: Pred
val negation: Pred -> Pred

val toPredicate: T list -> Pred
val toPrefixes: Pred -> T list

