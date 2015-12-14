module Regex
open Common

/// Extended regular expressions with negation and intersection 
/// Characters classes are modelled using sets of locations
type T = 
    | Empty
    | Epsilon
    | Locs of Set<string> 
    | Concat of T list
    | Inter of T list
    | Union of T list
    | Negate of T
    | Star of T

    override this.ToString() =
        let addParens s = "(" + s + ")"
        match this with 
        | Empty -> "{}"
        | Epsilon -> ""
        | Locs S -> "[" + (Set.toList S |> List.joinBy ",") + "]"
        | Concat rs -> "concat " + (List.map (fun r -> r.ToString()) rs |> List.joinBy ";" |> addParens)
        | Inter rs -> "inter " + (List.map (fun r -> r.ToString()) rs |> List.joinBy " and " |> addParens)
        | Union rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy " or " |> addParens
        | Negate r -> "!(" + r.ToString() + ")"
        | Star r -> (r.ToString() |> addParens) + "*"

let rec rev re = 
    match re with 
    | Empty | Epsilon | Locs _ -> re
    | Concat rs -> Concat (List.rev rs |> List.map rev)
    | Inter rs -> Inter (List.map rev rs)
    | Union rs -> Union (List.map rev rs)
    | Negate r -> Negate (rev r)
    | Star r -> Star (rev r)

(* Smart constructors for partitioning regular expressions 
   according to a few algebraic laws. Nested negation and Kleene star 
   operators are removed. Intersection, union, and concatenation are 
   merged and stored as lists that are sorted in ascending order. *)

let rec insertOrdered rs r = 
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

let rec insertOrderedAll dups rs1 rs2 = 
    match rs2 with 
    | [] -> rs1
    | hd::tl -> 
        insertOrderedAll dups (insertOrdered rs1 hd) tl

let empty = Empty 

let epsilon = Epsilon

let locs s = Locs s

let loc s = Locs (Set.singleton s)

let star r = 
    match r with 
    | Star _ -> r
    | Epsilon -> Epsilon
    | Empty -> Epsilon
    | _ -> Star r

let negate alphabet r = 
    match r with 
    | Negate _ -> r
    | Locs s -> Locs (Set.difference alphabet s)
    | _ -> Negate r

let rec concat r1 r2 = 
    match r1, r2 with 
    | _, Empty -> Empty
    | Empty, _ -> Empty
    | _, Epsilon -> r1
    | Epsilon, _ -> r2
    | Concat rs1, Concat rs2 -> Concat (List.append rs1 rs2)
    | Concat rs, _ -> Concat (List.append rs [r2])
    | _, Concat rs -> Concat (r1::rs)
    | _, _ -> concat (Concat [r1]) r2

let concatAll res =
    match res with 
    | [] -> Empty
    | _ -> Common.List.fold1 concat res

(* TODO: negate empty == alphabet check for locs *)
let rec inter r1 r2 = 
    if r1 = r2 then r1 else
    match r1, r2 with 
    | _, Empty -> Empty
    | Empty, _ -> Empty
    | _, Negate Empty -> r1
    | Negate Empty, _ -> r2
    | Inter rs1, Inter rs2 -> Inter (insertOrderedAll false rs1 rs2)
    | Inter rs, _ -> Inter (insertOrdered rs r2)
    | _, Inter rs -> Inter (insertOrdered rs r1)
    | _, _ -> inter (Inter [r1]) r2

let interAll res = 
    match res with 
    | [] -> Empty
    | _ -> Common.List.fold1 inter res

let rec union r1 r2 =
    if r1 = r2 then r1 else
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

let unionAll res = 
    match res with 
    | [] -> Empty
    | _ -> Common.List.fold1 union res


/// Check if a regular expression denotes only single characters and 
/// if so, returns the set of characters it denotes
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

/// Check if a regular expression accepts the empty string
let rec nullable r = 
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

/// An overapproximation of the set of character classes
let conserv r s =
    seq {for x in Set.toSeq r do
            for y in Set.toSeq s do 
                yield Set.intersect x y} |> Set.ofSeq

/// Approximate the character classes for a regular expression
let rec dclasses alphabet r =
    match r with 
    | Empty | Epsilon -> Set.singleton alphabet
    | Locs s ->
        assert not (Set.isEmpty s)
        Set.ofList [s; Set.difference alphabet s]
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

/// Compute the derivative of a regular expression with respect to a character class.
/// Results in a new regular expression that matches remaining strings
let rec derivative alphabet a r = 
    match r with 
    | Epsilon | Empty -> empty
    | Locs s -> if s.Contains(a) then epsilon else empty
    | Concat rs -> 
        match rs with 
        | [] | [_] -> failwith "impossible"
        | x::y::tl ->
            let y = if List.isEmpty tl then y else Concat (y::tl)
            union (concat (derivative alphabet a x) y) (concat (nullable x) (derivative alphabet a y))
    | Inter rs -> 
        rs
        |> List.map (fun r -> derivative alphabet a r)
        |> List.fold1 inter
    | Union rs -> List.fold (fun acc r -> union acc (derivative alphabet a r)) empty rs
    | Negate r' -> negate alphabet (derivative alphabet a r')
    | Star r' -> concat (derivative alphabet a r') r

/// Standard deterministic finite automaton, implemented 
/// using maps ans sets for simplicity
type Automaton =
    {q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

/// Explore and construct an automaton in a depth-first fashion
let rec goto alphabet q (Q,trans) S = 
    let c = Set.minElement S
    let qc = derivative alphabet c q 
    if Set.exists ((=) qc) Q then 
        (Q, Map.add (q,S) qc trans)
    else
        let Q' = Set.add qc Q 
        let trans' = Map.add (q,S) qc trans
        explore alphabet Q' trans' qc

and explore alphabet Q trans q = 
    let charClasses = Set.remove Set.empty (dclasses alphabet q)
    Set.fold (goto alphabet q) (Q,trans) charClasses

/// Re-index states by integers starting from 0 rather than regular expressions
let indexStates (q0, Q, F, trans) = 
    let aQ = Set.toSeq Q
    let idxs = seq {for i in 0..(Seq.length aQ - 1) -> i}
    let idxMap = idxs |> Seq.zip aQ |> Map.ofSeq
    let q0' = Map.find q0 idxMap
    let Q' = Set.ofSeq idxs
    let F' = Set.map (fun q -> Map.find q idxMap) F
    let trans' = Map.fold (fun acc (re,c) v -> Map.add ((Map.find re idxMap),c) (Map.find v idxMap) acc) Map.empty trans
    (q0', Q', F', trans')

/// Build a DFA for a regular expression directly using regular 
/// expression derivatives. Works well with complement,
/// intersection, and character classes. Produces near-minimal DFAs
let makeDFA alphabet r = 
    let q0 = r
    let (Q, trans) = explore alphabet (Set.singleton q0) Map.empty q0
    let F = Set.filter (fun q -> nullable q = epsilon) Q
    let (q0', Q', F', trans') = indexStates (q0, Q, F, trans)
    {q0=q0'; Q=Q'; F=F'; trans=trans'}


/// Parameterize regular expression by an alphabet. Since f# does 
/// not support ML-style functors, different objects can use different 
/// alphabets. Client code must ensure a single object is used
type REBuilder(topo: Topology.T) = 
    let (inStates, outStates) = Topology.alphabet topo
    let inside = Set.map (fun (s: Topology.State) -> s.Loc) inStates
    let outside = Set.map (fun (s: Topology.State) -> s.Loc) outStates
    let alphabet = Set.union inside outside
    member __.Inside = if Set.isEmpty inside then empty else locs inside
    member __.Outside = if Set.isEmpty outside then empty else locs outside
    member __.Rev = rev
    member __.Empty = empty
    member __.Epsilon = epsilon
    member __.Loc = loc
    member __.Locs = locs
    member __.Concat = concat
    member __.Inter = inter
    member __.Union = union
    member __.ConcatAll = concatAll
    member __.UnionAll = unionAll
    member __.InterAll = interAll
    member __.Star = star
    member __.Negate = negate alphabet
    member __.MakeDFA = makeDFA alphabet

    /// Given an alphabet, find all significant starting characters using derivatives
    member __.StartingLocs r =
        Set.fold (fun acc a -> 
            if derivative alphabet a r <> Empty 
            then Set.add a acc 
            else acc 
        ) Set.empty alphabet

    member this.Path(ls) =
        this.ConcatAll (List.map this.Loc ls)

    member this.MaybeOutside() =
        this.Star this.Outside

    member this.MaybeInside() = 
        this.Star this.Inside

    member this.Internal() =
        this.Concat this.Inside (this.Star this.Inside)

    member this.External() =
        this.Concat this.Outside (this.Star this.Outside)

    member this.Any() =
        this.ConcatAll [this.MaybeOutside(); this.Internal(); this.MaybeOutside()]

    member this.Waypoint(x) =
        this.ConcatAll [this.MaybeOutside(); this.MaybeInside(); this.Loc x; this.MaybeInside(); this.MaybeOutside()]

    member this.Avoid(x) =
        this.Negate (this.ConcatAll [this.MaybeOutside(); this.MaybeInside(); this.Loc x; this.MaybeInside(); this.MaybeOutside()])

    member this.EndsAt(x) =
        if inside.Contains x then
            this.ConcatAll [this.MaybeOutside(); this.MaybeInside(); this.Loc x]
        else if outside.Contains x then
            this.ConcatAll [this.MaybeOutside(); this.Internal(); this.MaybeOutside(); this.Loc x]
        else failwith ("[Constraint Error]: Location " + x + " is not a valid topology location" )

    (* Relies on ill-formedness of (x out+ in+) when x is an internal location *)
    member this.StartsAt(x) = 
        if inside.Contains x then
            this.ConcatAll [this.Loc x;  this.Internal(); this.MaybeOutside()]
        else if outside.Contains x then 
            this.ConcatAll [this.Loc x;  this.MaybeOutside(); this.Internal() ;this.MaybeOutside()]
        else failwith ("[Constraint Error]: Location " + x + " is not a valid topology location" )

    member this.ValleyFree(xs) =
        let _, pol = 
            List.fold (fun (last, acc) x ->
                let tierx = this.UnionAll (List.map this.Loc x)
                match last with
                | None -> (Some tierx, acc)
                | Some last ->
                    let bad = this.ConcatAll [last; tierx; last]
                    let avoid = this.Negate (this.ConcatAll [this.MaybeOutside(); this.MaybeInside(); bad; this.MaybeInside(); this.MaybeOutside()])
                    (Some tierx, this.Inter acc avoid)
            ) (None, this.Any()) xs
        pol

    