module Regex

open Topology
open QuickGraph
open Util
open Util.Format
open System.Collections.Generic

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
        | Epsilon -> "\"\""
        | Locs S -> "[" + (Set.toList S |> List.joinBy ",") + "]"
        | Concat rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy ";" |> addParens
        | Inter rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy " and " |> addParens
        | Union rs -> List.map (fun r -> r.ToString()) rs |> List.joinBy " or " |> addParens
        | Negate r -> "!(" + r.ToString() + ")"
        | Star r -> (r.ToString() |> addParens) + "*"

type Automaton =
    {q0: int;
     Q: Set<int>; 
     F: Set<int>;
     trans: Map<int*Set<string>, int>}

     override this.ToString() =
        let header = "=======================\n"
        let states = sprintf "States: %s\n" (Util.Set.toString this.Q)
        let init = sprintf "Initial: %d\n" this.q0
        let final = sprintf "Final: %s\n" (Util.Set.toString this.F)
        let trans = 
            Map.fold (fun acc (q,S) v ->
                let t = sprintf "  State: %d, chars: %s ---> %d\n" q (Util.Set.toString S) v 
                acc + t) "" this.trans
        let trans = sprintf "Transitions:\n%s" trans
        sprintf "%s%s%s%s%s%s" header states init final trans header

let rec rev re = 
    match re with 
    | Empty | Epsilon | Locs _ -> re
    | Concat rs -> Concat (List.rev rs |> List.map rev)
    | Inter rs -> Inter (List.map rev rs)
    | Union rs -> Union (List.map rev rs)
    | Negate r -> Negate (rev r)
    | Star r -> Star (rev r)

let rec insertOrdered rs r = 
    match rs with 
    | [] -> [r]
    | rhd::rtl -> 
        let cmp = compare r rhd
        if cmp < 0 then r::rs
        elif cmp = 0 then rs
        else rhd::(insertOrdered rtl r)

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
    | _ -> Util.List.fold1 concat res

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
    | _ -> Util.List.fold1 inter res

let rec union r1 r2 =
    if r1 = r2 then r1 else
    match r1, r2 with
    (*
    (* rewrite x;y + x;z = x;(y+z) *)
    | x, Concat (hd::tl) when x = hd -> concat hd (union epsilon (concatAll tl))
    | Concat (hd::tl), x when x = hd -> concat hd (union epsilon (concatAll tl))
    | Concat (hd1::tl1), Concat (hd2::tl2) when hd1 = hd2 -> concat hd1 (union (concatAll tl1) (concatAll tl2))
    | x, Concat ys when (List.rev ys).Head = x -> concat (union epsilon (concatAll (List.rev (List.tail (List.rev ys))))) x
    | Concat ys, x when (List.rev ys).Head = x -> concat (union epsilon (concatAll (List.rev (List.tail (List.rev ys))))) x
    | Concat xs, Concat ys when (List.rev xs).Head = (List.rev ys).Head ->
        let revx, revy = List.rev xs, List.rev ys
        let tlx = List.rev (List.tail revx)
        let tly = List.rev (List.tail revy)
        concat (union (concatAll tlx) (concatAll tly)) (List.head revx)
    (* rewrite variants of 1 + y;y* = y* *)
    | Epsilon, Concat [y1; Star y2] when y1 = y2 -> star y2
    | Epsilon, Concat [Star y1; y2] when y1 = y2 -> star y2
    | Concat [y1; Star y2], Epsilon when y1 = y2 -> star y2
    | Concat [y1; Star y2], Epsilon when y1 = y2 -> star y2 *)

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
    | _ -> Util.List.fold1 union res

let rec nullable r = 
    match r with 
    | Epsilon -> epsilon
    | Locs _ -> empty
    | Empty -> empty
    | Concat rs | Inter rs -> 
        Util.List.fold (fun acc r -> inter acc (nullable r)) epsilon rs
    | Union rs -> 
        Util.List.fold (fun acc r -> union acc (nullable r)) empty rs
    | Star r -> epsilon
    | Negate r ->
        match nullable r with 
        | Empty -> epsilon
        | _ -> empty 

let conserv r s =
    seq {for x in r do
            for y in s do 
                yield Set.intersect x y} |> Set.ofSeq

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

let indexStates (q0, Q, F, trans) =
    let aQ = Set.toArray Q
    let mutable Q' = Set.empty
    let idxMap = Dictionary()
    for i = 0 to Array.length aQ - 1 do
        Q' <- Set.add i Q'
        idxMap.[aQ.[i]] <- i
    let q0' = idxMap.[q0]
    let F' = Set.map (fun q -> idxMap.[q]) F
    let trans' = Map.fold (fun acc (re,c) v -> Map.add (idxMap.[re],c) idxMap.[v] acc) Map.empty trans
    (q0', Q', F', trans')

let makeDFA alphabet r = 
    let q0 = r
    let (Q, trans) = explore alphabet (Set.singleton q0) Map.empty q0
    let F = Set.filter (fun q -> nullable q = epsilon) Q
    let (q0', Q', F', trans') = indexStates (q0, Q, F, trans)
    {q0=q0'; Q=Q'; F=F'; trans=trans'}

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

let singleLocations (topo: Topology.T) r =
    let (ain, aout) = Topology.alphabet topo
    let rec inner r =
        let aux f r1 r2 = 
            match r1, r2 with 
            | None, _ -> None 
            | _, None -> None 
            | Some s1, Some s2 -> Some (f s1 s2)
        match r with
        | LIn -> Some (Set.map (fun (v: Topology.Node) -> v.Loc) ain)
        | LOut -> Some (Set.map (fun (v: Topology.Node) -> v.Loc) aout)
        | LLocs s -> Some s
        | LInter rs ->
            List.map inner rs |> 
            Util.List.fold1 (aux Set.intersect)
        | LUnion rs -> 
            List.map inner rs |>
            Util.List.fold1 (aux Set.union)
        | _ -> None
    inner r

let getAlphabet (topo: Topology.T) = 
    let (inStates, outStates) = Topology.alphabet topo
    let inside = Set.map (fun (s: Topology.Node) -> s.Loc) inStates
    let outside = Set.map (fun (s: Topology.Node) -> s.Loc) outStates
    let alphabet = Set.union inside outside
    (inside, outside, alphabet)

let emptinessAux dfa start = 
    let transitions = 
        dfa.trans
        |> Map.toSeq
        |> Seq.map (fun ((x,_),z) -> (x,z))
        |> Seq.groupBy fst
        |> Seq.map (fun (k,ss) -> (k, Set.ofSeq (Seq.map snd ss)))
        |> Map.ofSeq
    let s = System.Collections.Generic.Stack()
    let mutable marked = Set.singleton start
    let mutable edgeTo = Map.add start start Map.empty
    s.Push start
    while s.Count > 0 do 
        let v = s.Pop()
        for w in Map.find v transitions do
            if not (marked.Contains w) then
                edgeTo <- Map.add w v edgeTo
                marked <- Set.add w marked
                s.Push w
    let reachableFinal = Set.filter dfa.F.Contains marked
    if reachableFinal.Count > 0 then
        let mutable path = []
        let x = ref (Seq.head (Set.toSeq reachableFinal))
        while !x <> start do 
            let y = !x
            x := Map.find !x edgeTo
            let (_,ss) = Map.findKey (fun (i,ss) j -> i = !x && j = y) dfa.trans
            path <- (Set.minElement ss) :: path
        Some path
    else None

let emptiness (dfa: Automaton) : (string list) option =
    emptinessAux dfa dfa.q0

let startingLocs (dfa: Automaton) : Set<string> =
    let transitions = 
        dfa.trans
        |> Map.toSeq
        |> Seq.map (fun ((x,_),z) -> (x,z))
        |> Seq.groupBy fst
        |> Seq.map (fun (k,ss) -> (k, Set.ofSeq (Seq.map snd ss)))
        |> Map.ofSeq
    let s = System.Collections.Generic.Stack()
    let mutable marked = Set.singleton dfa.q0
    let mutable edgeTo = Map.add dfa.q0 dfa.q0 Map.empty
    s.Push dfa.q0
    while s.Count > 0 do 
        let v = s.Pop()
        for w in Map.find v transitions do
            if not (marked.Contains w) then
                edgeTo <- Map.add w v edgeTo
                marked <- Set.add w marked
                s.Push w
    let marked = marked
    Map.fold (fun acc (i,ss) j ->
        if dfa.F.Contains j && marked.Contains i then
            Set.union acc ss
        else acc
    ) Set.empty dfa.trans

   
type REBuilder(topo: Topology.T) =
    let unknownName = "out"
    let (ins, outs, alph) = getAlphabet topo
    let mutable inside = ins
    let mutable outside = Set.add unknownName outs
    let mutable alphabet = Set.add unknownName alph
    let mutable finalAlphabet = false
    let unknown: Topology.Node = Node(unknownName, Topology.Unknown)
    let topo =
        let t = Topology.copyTopology topo
        ignore (t.AddVertex unknown)
        for v in t.Vertices do
            if v.Typ = Topology.Outside then 
                Topology.addEdgesUndirected t [(v,unknown)]
        t

    let isInternal l = inside.Contains l

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

    member __.Alphabet() = alphabet

    member __.Topo() = topo

    member this.WellFormed(x) = 
        let r = this.Inter [this.Negate (this.Any()); x]
        let dfa = this.MakeDFA (convert r)
        emptiness dfa

    member this.Build (pred: Route.Predicate) (pref:int) re =
        finalAlphabet <- true
        match this.WellFormed re with
        | None -> convert re
        | Some cs ->
            // TODO: how to convert predicate to string?
            let msg = 
                sprintf "Invalid path shape for prefix %s, preference %d. " (string pred) pref + 
                sprintf "Paths must go through the internal network exactly once, " + 
                sprintf "but an example of a path that is allowed specified: %s" (string cs)
            error msg

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
            let v = Node(x, Topology.Outside)
            ignore (topo.AddVertex v)
            let allOutside = topo.Vertices |> Seq.filter Topology.isOutside
            for u in allOutside do
                if u <> v then
                    Topology.addEdgesUndirected topo [(u,v)]
        LLocs (Set.singleton x)

    member this.Locs (xs: string list) = 
        List.fold (fun acc x -> this.Union [acc; this.Loc x]) this.Empty xs

    member this.SingleLocations(r) = singleLocations (this.Topo()) r

    member __.MakeDFA r =
        assert (finalAlphabet)
        makeDFA alphabet r

    member __.StartingLocs dfa =
        if not finalAlphabet then 
            failwith "Builder must be final before computing starting locations"
        startingLocs dfa

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

    member this.Sigma() = 
        this.Union [this.Inside; this.Outside]

    member this.Always(xs) = 
        let locs = this.Locs xs
        this.Inter [this.Any(); this.Star locs]

    member this.Through(xs) =
        let (ins,outs) = List.partition isInternal xs
        this.Union 
            [this.Concat [this.MaybeOutside(); this.MaybeInside(); this.Locs ins; this.MaybeInside(); this.MaybeOutside()];
             this.Concat [this.MaybeOutside(); this.Internal(); this.MaybeInside(); this.MaybeOutside(); this.Locs outs; this.MaybeOutside()]]

    member this.Avoid(xs) =
        let (ins,outs) = List.partition isInternal xs
        let inLocs = this.Locs ins
        let outLocs = this.Locs outs
        let notInLocs = this.Inter [this.Negate inLocs; this.Inside]
        let notOutLocs = this.Inter [this.Negate outLocs; this.Outside]
        let valid = 
            this.Union
                [this.Concat [this.Star notInLocs; notInLocs; this.Star notOutLocs];
                 this.Concat [this.MaybeOutside(); notOutLocs; this.Star notInLocs; notInLocs; this.Star notOutLocs]]
        this.Inter [this.Any(); valid]

    member this.End(xs) =
        let (ins,outs) = List.partition isInternal xs
        this.Union
            [this.Concat [this.MaybeOutside(); this.MaybeInside(); this.Locs ins];
             this.Concat [this.MaybeOutside(); this.Internal(); this.MaybeOutside(); this.Locs outs]]

    member this.Start(xs) =
        let (ins,outs) = List.partition isInternal xs
        this.Union
            [this.Concat [this.Locs ins;  this.MaybeInside(); this.MaybeOutside()];
             this.Concat [this.Locs outs;  this.MaybeOutside(); this.Internal(); this.MaybeOutside()]]

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

    member this.Enter(xs) =
        let (ins,outs) = List.partition isInternal xs
        this.Union
            [this.Concat [this.External(); this.Locs ins; this.MaybeInside(); this.MaybeOutside()];
             this.Concat [this.MaybeOutside(); this.Locs outs; this.Internal(); this.MaybeOutside()]]

    member this.Exit(xs) =
        let (ins,outs) = List.partition isInternal xs 
        this.Union 
            [this.Concat [this.MaybeOutside(); this.MaybeInside(); this.Locs ins; this.External()];
             this.Concat [this.MaybeOutside(); this.Internal(); this.Locs outs; this.MaybeOutside()]]
    
    member this.Only(xs) = this.Concat [xs; this.Star xs]


module Test =

    let testRegexWellFormedness () =
        writeFormatted "Regex well-formedness "
        let pb = Route.PredicateBuilder()
        let reb = REBuilder (Examples.topoStretchingManWAN ())
        let pref1 = reb.Path ["X"; "B"; "X"; "B"]
        let pref2 = reb.Concat [reb.External(); reb.Internal(); reb.External(); reb.Internal()]
        let pref3 = reb.Concat [reb.Internal(); reb.External(); reb.Internal()]
        let mutable fail = false
        for p in [pref1; pref2; pref3] do
            try 
                ignore (reb.Build pb.True 1 p)
                fail <- true
            with _ -> ()
        if fail then failed () else passed ()

    let run () = 
        testRegexWellFormedness ()