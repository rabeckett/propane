module Bitset32

type T = int

let inline checkIndex i = 
    if i >= 32 then failwith "Invalid index"

let inline get (x:T) i : bool = 
    checkIndex i
    ((1 <<< i) &&& x) <> 0

let inline set (x:T) i : T = 
    checkIndex i
    (1 <<< i) ||| x

let inline clear (x:T) i : T = 
    checkIndex i
    (~~~(1 <<< i)) &&& x

let empty : T = 0

let singleton i : T =
    set empty i

let inline union (x:T) (y:T) : T = 
    x ||| y

let inline intersect (x:T) (y:T) : T = 
    x &&& y

let inline not (x:T) : T = 
    ~~~ x

let inline difference (x:T) (y:T) : T = 
    intersect x (not y)

let inline isEmpty (x:T) : bool = 
    (x = 0)

let inline count (x:T) = 
    let mutable x = x
    x <- x - ((x >>> 1) &&& 0x55555555)
    x <- (x &&& 0x33333333) + ((x >>> 2) &&& 0x33333333)
    ((x + (x >>> 4) &&& 0xF0F0F0F) * 0x1010101) >>> 24

let minimum (x:T) =
    let rec aux i =
        if i >= 32 then None 
        elif get x i then Some i
        else aux (i+1)
    aux 0

let toSet (x:T) = 
    let mutable ret = Set.empty 
    for i = 0 to 31 do 
        if get x i then 
            ret <- Set.add i ret
    ret