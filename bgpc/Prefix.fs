module Prefix 

(* TODO: 64 bit case *)

[<StructuralEquality; StructuralComparison>]
type T =
    {X1: uint32; X2: uint32; X3: uint32; X4: uint32; Slash: uint32}
    override this.ToString() =
        (string this.X1) + "." + 
        (string this.X2) + "." + 
        (string this.X3) + "." + 
        (string this.X4) + "/" + 
        (string this.Slash) 

type Pred = Pred of (uint32 * uint32) list

let prefix (a,b,c,d) slash = 
    {X1=a; X2=b; X3=c; X4=d; Slash=slash}

let str (Pred x) =
    sprintf "%A" x

let fromRange (x,y) = Pred [(x,y)]

let wholeRange = (uint32 0, System.UInt32.MaxValue)

let wfRange (x,y) = 
    x <= y

let overlap r1 r2 =
    assert (wfRange r1)
    assert (wfRange r2)
    let (a,b) = r1
    let (c,d) = r2
    (a >= c && a <= d) ||
    (c >= a && c <= b)

let touch r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    let (a,b) = r1
    let (c,d) = r2
    ((b = c-1u) && c <> 0u) || ((d = a-1u) && a <> 0u)

let isSmaller r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    let (a,b) = r1
    let (c,d) = r2
    b < c

let mergeUnion r1 r2 = 
    assert (wfRange r1)
    assert (wfRange r2)
    assert (overlap r1 r2 || touch r1 r2)
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
        else if touch r s then 
            union (mergeUnion r s) tl
        (* else if isSmaller r s then r::rs *)
        else s::(union r tl)

let rec disj (Pred rs1) (Pred rs2) = 
     match rs1 with 
     | [] -> (Pred rs2)
     | r::rs -> 
        disj (Pred rs) (Pred (union r rs2)) 

let rec inter r rs = 
    match rs with 
    | [] -> [] 
    | s::tl -> 
        if overlap r s then 
            (mergeInter r s)::(inter r tl)
        (* else if isSmaller r s then [] *)
        else  inter r tl

let rec conj (Pred rs1) (Pred rs2) = 
    match rs1 with
    | [] -> Pred []
    | r::rs ->
        let x = Pred (inter r rs2)
        let y = conj (Pred rs) (Pred rs2)
        disj x y

let bot = Pred []

let top = Pred [wholeRange]

let negate r =
    assert (wfRange r)
    let (x,y) = r 
    let (a,b) = wholeRange
    match x=a, y=b with
    | true, true -> []
    | true, false -> [(y+1u,b)]
    | false, true -> [(a,x-1u)]
    | false, false -> [(a,x-1u); (y+1u,b)]

let rec negation (Pred rs) =
    match rs with 
    | [] -> Pred [wholeRange]
    | r::tl ->
        let x = Pred (negate r) 
        let y = negation (Pred tl) 
        conj x y

let inline shr x bits = 
    if bits >= 32 then 0u else x >>> bits

let inline shl x bits =
    if bits >= 32 then 0u else x <<< bits

let isOne x i =
    shr (shl x i) 31 = 1u

let binaryStr x =
    let mutable result = "" 
    for i = 0 to 31 do
        if i % 8 = 0 then 
            result <- result + " "
        result <- result + (if isOne x i then "1" else "0")
    result

let rangeOfPrefix (p: T) =
    let lowermask = shl 0xffffffffu (32 - int p.Slash)
    let uppermask = shr 0xffffffffu (int p.Slash)
    let value = (shl p.X1 24) + (shl p.X2 16) + (shl p.X3 8) + p.X4
    let value = value &&& uint32 lowermask
    (value, value + uint32 uppermask)

let toPredicate (ps: T list) : Pred =
    List.map rangeOfPrefix ps
    |> List.fold (fun acc x -> disj acc (Pred [x]) ) bot

let inline dotted x = 
    let a = shr x 24
    let b = shr (shl x 8) 24
    let c = shr (shl x 16) 24
    let d = shr (shl x 24) 24
    (a, b, c, d)

let inline firstNBits x n = 
    x &&& (shl 0xffffffffu (32 - int n))

let rec prefixesOfRange r : T list =
    let without p =
        let r' = rangeOfPrefix p
        let (Pred remaining) = conj (Pred (negate r')) (Pred [r])
        remaining
        |> List.map prefixesOfRange
        |> List.collect id
    assert (wfRange r)
    let (a,b) = r
    let mutable lastOneA = None
    let mutable lastZeroB = None
    let mutable lastOneB = None
    for i = 0 to 31 do
        if isOne a i then
            lastOneA <-  Some i
        if isOne b i then
            lastOneB <- Some i
        else
            lastZeroB <-  Some i
    match lastOneA, lastZeroB with
    | None, None -> [prefix (0u,0u,0u,0u) 0u] 
    | None, Some i -> 
        if i <> 31 then 
            let j = uint32 i + 1u
            let (a,b,c,d) = dotted (firstNBits b j)
            let rng = prefix (a,b,c,d) j
            rng :: without rng
        else
            let (a,b,c,d) = dotted b
            let rng = prefix (a,b,c,d) 32u 
            rng :: without rng
    | Some i, None ->
        let (a,b,c,d) = dotted a
        let j = uint32 i + 1u
        let rng = prefix (a,b,c,d) j
        rng :: without rng
    | Some i, Some j ->
        (* everything after i in a is 000000s *)
        (* everything after j in b is 111111s *)
        let k = max i j
        let slash = uint32 k + 1u
        let (a,b,c,d) = dotted (firstNBits (if i >= j then a else b) slash)
        let rng = prefix (a,b,c,d) slash
        rng :: without rng

let toPrefixes (Pred rs: Pred) : T list =
    rs
    |> List.map prefixesOfRange
    |> List.concat