module Bitset32

type T = int

let validIndex i = 
    (i >= 0 && i < 32)

let inline get (x:T) i : bool = 
    assert (validIndex i)
    ((1 <<< i) &&& x) <> 0

let inline set (x:T) i : T = 
    assert (validIndex i)
    (1 <<< i) ||| x

let inline clear (x:T) i : T = 
    assert (validIndex i)
    (~~~(1 <<< i)) &&& x

let empty : T = 0

let inline singleton i : T =
    set empty i

let inline union (x:T) (y:T) : T = 
    x ||| y

let inline intersect (x:T) (y:T) : T = 
    x &&& y

let inline negate (x:T) : T = 
    ~~~ x

let inline difference (x:T) (y:T) : T = 
    intersect x (negate y)

let inline isEmpty (x:T) : bool = 
    (x = 0)

let inline count (x:T) = 
    let mutable x = x
    x <- x - ((x >>> 1) &&& 0x55555555)
    x <- (x &&& 0x33333333) + ((x >>> 2) &&& 0x33333333)
    ((x + (x >>> 4) &&& 0xF0F0F0F) * 0x1010101) >>> 24

let minimum (x:T) =
    let rec aux i v =
        if i >= 32 then None 
        elif (v &&& 1) = 1 then Some i
        else aux (i+1) (v >>> 1)
    aux 0 x

let inline iter f (x:T) = 
    let mutable v = x
    for i = 0 to 31 do
        if (v &&& 1) = 1 then 
            f i 
        v <- (v >>> 1)

let inline fold f b (x:T) =
    let mutable v = x 
    let mutable acc = b
    for i = 0 to 31 do 
        if (v &&& 1) = 1 then
            acc <- f acc i 
        v <- (v >>> 1)
    acc

let inline filter f (x:T) =
    let mutable v = x
    let mutable acc = x
    for i = 0 to 31 do
        if (v &&& 1) = 1 && not (f i) then
            acc <- clear acc i
        v <- (v >>> 1)
    acc