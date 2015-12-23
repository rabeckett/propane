module Regex

open Topology
open QuickGraph
open Common
open Common.Error

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
        | Concat rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy ";" |> addParens
        | Inter rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy " and " |> addParens
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


/// Representation for a regex that we haven't built yet. 
/// Since we don't have the complete alphabet until we have built the entire
/// regular expression (due to partial AS topology information), we delay
/// the construction until the Build method is called in the builder object below.
type LazyT =
    | LIn
    | LOut
    | LEmpty
    | LEpsilon
    | LLocs of Set<string> 
    | LConcat of LazyT list
    | LInter of LazyT list
    | LUnion of LazyT list
    | LNegate of LazyT
    | LStar of LazyT

let rec singleLocations alphabet r =
    let aux f r1 r2 = 
        match r1, r2 with 
        | None, _ -> None 
        | _, None -> None 
        | Some s1, Some s2 -> Some (f s1 s2)
    match r with 
    | LLocs s -> Some s
    | LInter rs ->
        List.map (singleLocations alphabet) rs |> 
        Common.List.fold1 (aux Set.intersect)
    | LUnion rs -> 
        List.map (singleLocations alphabet) rs |>
        Common.List.fold1 (aux Set.union)
    | LNegate r ->
        Option.map (Set.difference alphabet) (singleLocations alphabet r)
    | _ -> None

let getAlphabet (topo: Topology.T) = 
    let (inStates, outStates) = Topology.alphabet topo
    let inside = Set.map (fun (s: Topology.State) -> s.Loc) inStates
    let outside = Set.map (fun (s: Topology.State) -> s.Loc) outStates
    let alphabet = Set.union inside outside
    (inside, outside, alphabet)

/// Parameterize regular expression by an alphabet. Since f# does
/// not support ML-style functors, different objects can use different
/// alphabets. Client code must ensure a single object is used.
type REBuilder(topo: Topology.T) =
    let unknownName = "out"
    let (ins, outs, alph) = getAlphabet topo
    let mutable inside = ins
    let mutable outside = Set.add unknownName outs
    let mutable alphabet = Set.add unknownName alph
    let mutable isDone = false

    (* Add the unknown special node to the topology *)
    let unknown: Topology.State = {Loc=unknownName; Typ = Topology.Unknown}
    let topo =
        ignore (topo.AddVertex unknown)
        for v in topo.Vertices do
            if v.Typ = Topology.Outside then 
                Topology.addEdgesUndirected topo [(v,unknown)]
        Topology.copyTopology topo

    (* let isExternal l = String.length l > 2 && l.[0] = 'A' && l.[1] = 'S' *)
    let check alphabet l =
        if not (Set.contains l alphabet) then
            error (sprintf "invalid topology location: %s" l)
    
    (* Invariant: Build has set the alphabet *)
    let rec convert (re: LazyT) : T = 
        match re with 
        | LIn -> if Set.isEmpty inside then empty else locs inside
        | LOut -> if Set.isEmpty outside then empty else locs outside
        | LEmpty -> empty
        | LEpsilon -> epsilon
        | LLocs ls -> locs ls 
        | LConcat xs -> concatAll (List.map convert xs)
        | LInter xs -> interAll (List.map convert xs)
        | LUnion xs -> unionAll (List.map convert xs)
        | LNegate x -> negate alphabet (convert x) 
        | LStar x -> star (convert x)

    member __.Topo() = topo

    (* Creates the actual regex now that we have the full alphabet and topology *)
    member this.Build re =
        isDone <- true
        convert re

    member __.Empty = LEmpty
    member __.Epsilon = LEpsilon
    member __.Concat xs = LConcat xs
    member __.Inter xs = LInter xs
    member __.Union xs = LUnion xs
    member __.Star x = LStar x
    member __.Negate x = LNegate x
    member __.Inside = LIn
    member __.Outside = LOut

    member __.Loc x =
        if not (alphabet.Contains x) then 
            outside <- Set.add x outside
            alphabet <- Set.add x alphabet
            let v = {Loc=x; Typ=Topology.Outside}
            topo.AddVertex v |> ignore
            let allOutside = topo.Vertices |> Seq.filter Topology.isOutside
            for u in allOutside do
                if u <> v then
                    Topology.addEdgesUndirected topo [(u,v)]
        LLocs (Set.singleton x)

    member __.MakeDFA r =
        assert (isDone)
        makeDFA alphabet r

    member __.StartingLocs r =
        assert (isDone)
        Set.fold (fun acc a -> 
            if derivative alphabet a r <> Empty 
            then Set.add a acc 
            else acc 
        ) Set.empty alphabet

    member this.Path(ls) =
        this.Concat (List.map this.Loc ls)

    member this.MaybeOutside() =
        this.Star this.Outside

    member this.MaybeInside() = 
        this.Star this.Inside

    member this.Internal() =
        this.Concat [this.Inside; (this.Star this.Inside)]

    member this.External() =
        this.Concat [this.Outside; (this.Star this.Outside)]

    member this.Any() =
        this.Concat [this.MaybeOutside(); this.Internal(); this.MaybeOutside()]

    member this.Waypoint(x) =
        this.Concat [this.MaybeOutside(); this.MaybeInside(); this.Loc x; this.MaybeInside(); this.MaybeOutside()]

    (* TODO: more efficient with character classes *)
    member this.WaypointAny(xs) =
        this.Union (List.map this.Waypoint xs)

    member this.Avoid(x) =
        this.Negate (this.Waypoint(x))

    (* TODO: more efficient with character classes *)
    member this.AvoidAny(xs) =
        this.Union (List.map this.Avoid xs)

    member this.EndsAt(x) =
        if inside.Contains x then
            this.Concat [this.MaybeOutside(); this.MaybeInside(); this.Loc x]
        else
            this.Concat [this.MaybeOutside(); this.Internal(); this.MaybeOutside(); this.Loc x]

    (* Relies on ill-formedness of (x out+ in+) when x is an internal location *)
    member this.StartsAt(x) = 
        if inside.Contains x then
            this.Concat [this.Loc x;  this.Internal(); this.MaybeOutside()]
        else
            this.Concat [this.Loc x;  this.MaybeOutside(); this.Internal() ;this.MaybeOutside()]

    (* TODO: use character classes to split by inside/outside (more efficient) *)
    member this.EndsAtAny(xs) =
        this.Union (List.map this.EndsAt xs)

    (* TODO: use character classes to split by inside/outside (more efficient) *)
    member this.StartsAtAny(xs) =
        this.Union (List.map this.StartsAt xs)

    member this.ValleyFree(xs) =
        let aux (last, acc) x =
            let tierx = this.Union (List.map this.Loc x)
            match last with
            | None -> (Some tierx, acc)
            | Some last ->
                let bad = this.Concat [tierx; last; tierx]
                let sq = [this.MaybeOutside(); this.MaybeInside(); bad; this.MaybeInside(); this.MaybeOutside()]
                let avoid = this.Negate (this.Concat sq)
                (Some tierx, this.Inter [acc; avoid])
        List.fold aux (None, this.Any()) xs |> snd