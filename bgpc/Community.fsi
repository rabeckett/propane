module Community


type Pred =
    | Bot
    | Val of Set<Set<string> * Set<string>>

val bot: Pred 
val top: Pred
val value: string -> Pred
val disj: Pred -> Pred -> Pred
val conj: Pred -> Pred -> Pred
val negate: Pred -> Pred