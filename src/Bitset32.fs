module Bitset32

open LanguagePrimitives


[<Measure>]
type B

type T = int<B>


let inline validIndex i = 
    (i >= 0 && i < 32)

let empty : T = 0<B>

let inline contains i (x:T) : bool = 
    assert (validIndex i)
    ((1 <<< i) &&& int x) <> 0

let inline add i (x:T) : T = 
    assert (validIndex i)
    Int32WithMeasure<B> ( (1 <<< i) ||| int x )

let inline remove i (x:T) : T = 
    assert (validIndex i)
    Int32WithMeasure<B> ((~~~(1 <<< i)) &&& int x)

let inline singleton i : T =
    assert (validIndex i)
    add i empty

let inline union (x:T) (y:T) : T = 
    Int32WithMeasure<B> (int x ||| int y)

let inline intersect (x:T) (y:T) : T = 
    Int32WithMeasure<B> (int x &&& int y)

let inline negate (x:T) : T = 
    Int32WithMeasure<B> (~~~ (int x))

let inline difference (x:T) (y:T) : T = 
    intersect x (negate y)

let inline isEmpty (x:T) : bool = 
    (int x = 0)

let inline count (x:T) = 
    let mutable x = int x
    x <- x - ((x >>> 1) &&& 0x55555555)
    x <- (x &&& 0x33333333) + ((x >>> 2) &&& 0x33333333)
    ((x + (x >>> 4) &&& 0xF0F0F0F) * 0x1010101) >>> 24

let inline minimum (x:T) =
    let rec aux i v =
        if i >= 32 then None 
        elif (v &&& 1) = 1 then Some i
        else aux (i+1) (v >>> 1)
    aux 0 (int x)

let inline iter f (x:T) = 
    let mutable v = int x
    for i = 0 to 31 do
        if (v &&& 1) = 1 then 
            f i 
        v <- (v >>> 1)

let inline fold f b (x:T) =
    let mutable v = int x 
    let mutable acc = b
    for i = 0 to 31 do 
        if (v &&& 1) = 1 then
            acc <- f acc i 
        v <- (v >>> 1)
    acc

let inline filter f (x:T) =
    let mutable v = int x
    let mutable acc = x
    for i = 0 to 31 do
        if (v &&& 1) = 1 && not (f i) then
            acc <- remove i acc
        v <- (v >>> 1)
    acc

let inline exists f (x:T) =
    let rec aux i v =
        if i >= 32 then false 
        elif (v &&& 1) = 1 then
            if f i then true 
            else aux (i+1) (v >>> 1)
        else aux (i+1) (v >>> 1)
    aux 0 (int x)

let inline forall f (x:T) =
    let rec aux i v =
        if i >= 32 then true 
        elif (v &&& 1) = 1 then
            if f i then aux (i+1) (v >>> 1) 
            else false
        else aux (i+1) (v >>> 1)
    aux 0 (int x)