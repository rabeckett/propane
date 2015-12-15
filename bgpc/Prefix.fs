module Prefix 

(* TODO: make these 16 bits *)

type T = int * int * int * int * int option

type Ranges = (int * int) list


let wholeRange = (System.Int32.MinValue, System.Int32.MaxValue)

let wfRange (x,y) = 
    x <= y

let overlap r1 r2 =
    assert (wfRange r1)
    assert (wfRange r2)
    let (x1,y1) = r1
    let (x2,y2) = r2
    (x1 >= x2 && x1 <= y2) ||
    (x2 >= x1 && x2 <= y1)

let isSmaller r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    let (a,b) = r1
    let (c,d) = r2
    b < c

let mergeUnion r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    assert (overlap r1 r2)
    let (a,b) = r1 
    let (c,d) = r2
    (min a c, max b d)

let mergeInter r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    assert (overlap r1 r2)
    let (a,b) = r1 
    let (c,d) = r2
    (max a c, min b d)

let rec union r rs =
    match rs with
    | [] -> [r]
    | s::tl ->
        if overlap r s then 
            union (mergeUnion r s) tl
        else if isSmaller r s then 
            r::rs
        else s::(union r tl)

let rec unionAll rs1 rs2 = 
     match rs1 with 
     | [] -> rs2
     | r::rs -> 
        unionAll rs (union r rs2) 

let rec inter r rs = 
    match rs with 
    | [] -> [] 
    | s::tl -> 
        if overlap r s then 
            (mergeInter r s)::(inter r tl)
        else if isSmaller r s then []
        else inter r tl

let rec interAll rs1 rs2 = 
    match rs1 with
    | [] -> []
    | r::rs -> 
        interAll (inter r rs2) rs

let negate r =
    let (x,y) = r 
    let (a,b) = wholeRange
    let foo = if y = b then [(a,x)] else []
    let bar = if a = x then [(y,b)] else []
    foo @ bar

let rec negateAll rs =
    match rs with 
    | [] -> [wholeRange]
    | r::tl ->
        interAll (negate r) (negateAll tl)

let rangeOfPrefix (a,b,c,d) bits = 
    let lowermask = 0xffffffff <<< (32 - bits)
    let upper = 0xffffffff >>> bits
    let value = ((a <<< 24) + (b <<< 16) + (c <<< 8) + d) &&& lowermask
    (value, value + upper)