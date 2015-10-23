module Regex

(* Extended regular expressions with negation and intersection 
   Characters are modelled using sets of locations as character classes *)
type T = 
    | Empty
    | Epsilon
    | Locs of Set<string> 
    | Concat of T list
    | Inter of T list
    | Union of T list
    | Negate of T
    | Star of T

(* Return a regular expression that matches reversed strings *)
let rec private rev re = 
    match re with 
    | Empty | Epsilon | Locs _ -> re
    | Concat rs -> Concat (List.rev rs |> List.map rev)
    | Inter rs -> Inter (List.map rev rs)
    | Union rs -> Union (List.map rev rs)
    | Negate r -> Negate (rev r)
    | Star r -> Star (rev r)


(* Smart constructors for building partitioning regular expressions 
   according to a few algebraic laws. Nested negation and Kleene star 
   operators are removed. Intersection, union, and concatenation are 
   merged and stored as lists that are sorted in ascending order. *)

let rec private insertOrdered rs r = 
    match rs with 
    | [] -> [r]
    | rhd::rtl -> 
        let cmp = compare r rhd
        if cmp < 0 then
            r::rs
        else if cmp = 0 then
            rs
        else 
            rhd::(insertOrdered rtl r)

let rec private insertOrderedAll dups rs1 rs2 = 
    match rs2 with 
    | [] -> rs1
    | hd::tl -> 
        insertOrderedAll dups (insertOrdered rs1 hd) tl

let private empty = Empty 

let private epsilon = Epsilon

let private locs s = Locs s

let private loc s = Locs (Set.singleton s)

let private star r = 
    match r with 
    | Star _ -> r
    | Epsilon -> Epsilon
    | Empty -> Epsilon
    | _ -> Star r

let private negate alphabet r = 
    match r with 
    | Negate _ -> r
    | Locs s -> Locs (Set.difference alphabet s)
    | _ -> Negate r

let rec private concat r1 r2 = 
    match r1, r2 with 
    | _, Empty -> Empty
    | Empty, _ -> Empty
    | _, Epsilon -> r1
    | Epsilon, _ -> r2
    | Concat rs1, Concat rs2 -> Concat (List.append rs1 rs2)
    | Concat rs, _ -> Concat (List.append rs [r2])
    | _, Concat rs -> Concat (r1::rs)
    | _, _ -> concat (Concat [r1]) r2

let rec private inter r1 r2 = 
    match r1, r2 with 
    | _, Empty -> Empty
    | Empty, _ -> Empty
    | _, Negate Empty -> r1
    | Negate Empty, _ -> r2
    | Inter rs1, Inter rs2 -> Inter (insertOrderedAll false rs1 rs2)
    | Inter rs, _ -> Inter (insertOrdered rs r2)
    | _, Inter rs -> Inter (insertOrdered rs r1)
    | _, _ -> inter (Inter [r1]) r2

let rec private union r1 r2 = 
    match r1, r2 with 
    | _, Empty -> r1
    | Empty, _ -> r2
    | _, Negate Empty -> r2 
    | Negate Empty, _ -> r1
    | Locs r, Locs s -> Locs (Set.union r s)
    | Union rs1, Union rs2 -> Union (insertOrderedAll false rs1 rs2)
    | Union rs, _ -> Union (insertOrdered rs r2)
    | _, Union rs -> Union (insertOrdered rs r1)
    | _, _ -> union (Union [r1]) r2


(* Check if a regular expression denotes only single characters and 
   if so, return the set of characters it denotes. *)
let rec singleLocations alphabet r =
    let aux f r1 r2 = 
        match r1, r2 with 
        | None, _ -> None 
        | _, None -> None 
        | Some s1, Some s2 -> Some (f s1 s2)

    match r with 
    | Locs s -> Some s
    | Inter rs ->
        List.map (singleLocations alphabet) rs |> 
        Common.List.fold1 (aux Set.intersect)
    | Union rs -> 
        List.map (singleLocations alphabet) rs |>
        Common.List.fold1 (aux Set.union)
    | Negate r ->
        Option.map (Set.difference alphabet) (singleLocations alphabet r)
    | _ -> None


(* Build a DFA for a regular expression directly using regular 
   expression derivatives. Works well with complement,
   intersection, and character classes. Produces near-minimal DFAs *)

(* Check if a regular expression accepts the empty string *)
let rec private nullable r = 
    match r with 
    | Epsilon -> epsilon
    | Locs _ -> empty
    | Empty -> empty
    | Concat rs | Inter rs -> 
        List.fold (fun acc r -> inter acc (nullable r)) epsilon rs
    | Union rs -> 
        List.fold (fun acc r -> union acc (nullable r)) empty rs
    | Star r -> epsilon
    | Negate r ->
        match nullable r with 
        | Empty -> epsilon
        | _ -> empty 

(* An overapproximation of the set of character classes *)
let private conserv r s =
    seq {for x in Set.toSeq r do
            for y in Set.toSeq s do 
                yield Set.intersect x y} |> Set.ofSeq

(* Approximate the character classes for a regular expression *)
let rec private dclasses alphabet r =
    match r with 
    | Empty | Epsilon -> Set.singleton alphabet
    | Locs s ->
        assert not (Set.isEmpty s)
        let diff = Set.difference alphabet s 
        if Set.isEmpty diff then 
            Set.ofList [s]
        else 
            Set.ofList [s; diff]
    | Concat rs ->
        match rs with 
        | [] -> failwith "impossible"
        | [r] -> dclasses alphabet r
        | r::tl -> 
            if nullable r = empty then 
                dclasses alphabet r 
            else 
                conserv (dclasses alphabet r) (dclasses alphabet (Concat tl))
    | Inter rs | Union rs -> 
        List.fold (fun acc r -> conserv acc (dclasses alphabet r)) (Set.singleton alphabet) rs
    | Star r -> dclasses alphabet r 
    | Negate r -> dclasses alphabet r

(* Compute the derivative of a regular expression with respect to a character class *)
let rec private derivative alphabet a r = 
    match r with 
    | Epsilon | Empty -> empty
    | Locs s -> if Set.contains a s then epsilon else empty
    | Concat rs -> 
        match rs with 
        | [] -> failwith "impossible"
        | x::[] -> failwith "impossible"
        | x::y::tl ->
            let y = if List.isEmpty tl then y else Concat (y::tl)
            union (concat (derivative alphabet a x) y) (concat (nullable x) (derivative alphabet a y))
    | Inter rs -> List.fold (fun acc r -> inter acc (derivative alphabet a r)) empty rs
    | Union rs -> List.fold (fun acc r -> union acc (derivative alphabet a r)) empty rs
    | Negate r' -> negate alphabet (derivative alphabet a r')
    | Star r' -> concat (derivative alphabet a r') r

type Automata =
    {pref: int;
     q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

(* Explore and construct the automata in a depth-first fashion *)
let rec private goto alphabet q (Q,trans) S = 
    let c = Set.minElement S
    let qc = derivative alphabet c q 
    if Set.exists ((=) qc) Q then 
        (Q, Map.add (q,S) qc trans)
    else
        let Q' = Set.add qc Q 
        let trans' = Map.add (q,S) qc trans
        explore alphabet Q' trans' qc

and private explore alphabet Q trans q = 
    Set.fold (goto alphabet q) (Q,trans) (dclasses alphabet q)

(* Re-index states by ints rather than regular expressions *)
let private indexStates (q0, Q, F, trans) = 
    let aQ = Set.toSeq Q
    let idxs = seq {for i in 0..(Seq.length aQ - 1) -> i}
    let idxMap = idxs |> Seq.zip aQ |> Map.ofSeq
    let q0' = Map.find q0 idxMap
    let Q' = Set.ofSeq idxs
    let F' = Set.map (fun q -> Map.find q idxMap) F
    let trans' = Map.fold (fun acc (re,c) v -> Map.add ((Map.find re idxMap),c) (Map.find v idxMap) acc) Map.empty trans
    (q0', Q', F', trans')

(* Build a DFA over a custom alphabet using derivatives *)
let private makeDFA alphabet pref r = 
    let q0 = r
    let (Q, trans) = explore alphabet (Set.singleton q0) Map.empty q0
    let F = Set.filter (fun q -> nullable q = epsilon) Q 
    let (q0', Q', F', trans') = indexStates (q0, Q, F, trans)
    {pref=pref; q0=q0'; Q=Q'; F=F'; trans=trans'}


(* Parameterize regular expression by an alphabet. Since f# does 
   not support ML-style functors, different objects can use different 
   alphabets. Client code must ensure a single object is used. *)
type REBuilder(inside, outside) = 

    let alphabet = Set.union inside outside
    
    member __.Inside = locs inside
    member __.Outside = locs outside
    member __.Rev = rev
    member __.Empty = empty
    member __.Epsilon = epsilon
    member __.Loc = loc
    member __.Locs = locs
    member __.Concat = concat
    member __.Inter = inter
    member __.Union = union
    member __.Star = star
    member __.Negate = negate alphabet
    member __.MakeDFA = makeDFA alphabet

    