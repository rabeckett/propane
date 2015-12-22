module Prefix


type T =
    val X1: uint32
    val X2: uint32
    val X3: uint32
    val X4: uint32
    val Slash: uint32
    new: uint32 * uint32 * uint32 * uint32 * uint32 -> T
    override ToString: unit -> string

type Pred = (uint32 * uint32)  list

val disj: Pred -> Pred -> Pred
val conj: Pred -> Pred -> Pred
val bot: Pred
val top: Pred
val negation: Pred -> Pred

val toPredicate: T list -> Pred
val toPrefixes: Pred -> T list

