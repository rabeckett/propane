module FastPredicate

open System.Collections.Generic


/// Multi-terminal Binary decision diagram to represent predicates
/// over router prefixes, communities, topology locations etc.
///
/// Bdd nodes are hash consed for unicity, and have terminal 
/// nodes that represent a prefix length range.
/// 
/// For example, the prefix predicate:  (1.2.3.0/8 ge 16 le 24)
/// would have bdd variables for each bit of the prefix, and 
/// a terminal node representing the length [16..24]

let memoize f =
    let unq = Dictionary()
    (fun x -> 
        if unq.ContainsKey(x) then unq.[x]
        else 
            let res = f x 
            unq.[x] <- res; res)

type Var = int


/// Simple range data structure representing 
/// a continuous range of valid prefix lengths 
/// between 0 and 32

type Range = struct 
    val Lo: int
    val Hi: int
    new(l,h) = {Lo = l; Hi = h}

    member this.IsEmpty = this.Lo < 0

    member r1.Union(r2: Range) =
        if r1.IsEmpty then r2 
        elif r2.IsEmpty then r1 
        else Range(min r1.Lo r2.Lo, max r1.Hi r2.Hi)

    member r1.Inter(r2: Range) = 
        if r1.IsEmpty || r1.IsEmpty || r1.Lo > r2.Hi || r2.Lo > r1.Hi 
        then Range.Empty
        else Range(max r1.Lo r2.Lo, min r1.Hi r2.Hi)

    member r.Negate() = 
        if r.IsEmpty then Range.Full 
        else
            match r.Lo, r.Hi with 
            | 0, 32 -> Range.Empty 
            | -1, -1 -> Range.Full
            | 0, x -> Range(x+1,32)
            | x, 32 -> Range(0, x-1)
            | _ -> failwith "Invalid range negation"

    override r.ToString() = sprintf "%d..%d" r.Lo r.Hi
    static member Empty = Range(-1,-1)
    static member Full = Range(0,32)
end


/// Bdd nodes are hash consed to ensure maximal sharing.
/// This enables O(1) hashing, comparison, equality,
/// satisfiability and validity checks.

[<CustomComparison; CustomEquality>]
type HashCons = struct
    val Id: int
    val Node: Node
    new(id, node) = {Id = id; Node = node}

    override x.Equals(other) =
        match other with
        | :? HashCons as y -> (x.Id = y.Id)
        | _ -> false
 
    override x.GetHashCode() = x.Id
     
    interface System.IComparable with
        member x.CompareTo other =
          match other with
          | :? HashCons as y -> x.Id - y.Id
          | _ -> failwith "cannot compare values of different types"
end 

/// Multi-terminal Bdd node. We store prefix len ranges in 
/// the leaves, which is more efficient than encoding 
/// the range directly in binary.
///
/// Pointers to adjacent nodes are stored directly to avoid 
/// lookup going through a hash table indirection 

and Node = 
    | Leaf of Range
    | Node of Var * HashCons * HashCons


/// Bdd builder object, which stores Bdd nodes in 
/// a unique table. The variable order can be provided, 
/// but it does no variable reordering.

type BddBuilder(order : Var -> Var -> int) =
    let uniqueTab = Dictionary<Node,HashCons>()
    let mutable nextIdx = 1

    let mkNode(node: Node) : HashCons =
        if uniqueTab.ContainsKey(node) then 
            uniqueTab.[node]
        else
            let idx = nextIdx 
            let hconsNode = HashCons(idx, node)
            uniqueTab.[node] <- hconsNode 
            nextIdx <- nextIdx + 1 
            hconsNode

    let leaf v = mkNode(Leaf v)

    let node(v, l, r) = 
        if l = r then l 
        else mkNode(Node(v,l,r))

    let trueNode = leaf Range.Full
    let falseNode = leaf Range.Empty

    let rec negate (n: HashCons) = 
        match n.Node with 
        | Leaf v -> leaf (v.Negate())
        | Node(v,l,r) -> node(v, negate l, negate r)

    let rec apply f (n1: HashCons) (n2: HashCons) =
        match n1.Node, n2.Node with
        | Leaf v1, Leaf v2 -> leaf (f v1 v2)
        | Leaf _, Node(v,l,r) -> node(v, apply f l n1, apply f r n1)
        | Node(v,l,r), Leaf _ -> node(v, apply f l n2, apply f r n2)
        | Node(v1,l1,r1), Node(v2,l2,r2) -> 
            let v, (la,lb), (ra,rb) = 
                match order v1 v2 with 
                | c when c = 0 -> v1,(l1,l2),(r1,r2)
                | c when c < 0 -> v1,(l1,n2),(r1,n2)
                | c            -> v2,(n1,l2),(n1,r2)
            node(v, apply f la lb, apply f ra rb)

    let apply f = memoize (apply f)
    let opOr = apply (fun r1 r2 -> r1.Union(r2))
    let opAnd = apply (fun r1 r2 -> r1.Inter(r2))

    member __.NodeCount = nextIdx + 1
    member __.Value(v) = leaf v
    member __.Var(x) = node(x, trueNode, falseNode)
    member __.Ite(x,l,r) = node(x, l, r)
    member __.False = falseNode
    member __.True = trueNode
    member __.Or(n1, n2) = opOr n1 n2
    member __.And(n1, n2) = opAnd n1 n2
    member __.Not(n) = negate n
    member this.Implies(n1, n2) = this.Or(this.Not n1, n2) = falseNode
   
    member __.IterPath(f, x) = 
        let rec aux ts fs (n: HashCons) =
            match n.Node with
            | Node(v,l,r) ->  
                aux (Set.add v ts) fs l 
                aux ts (Set.add v fs) r
            | Leaf v -> f ts fs v
        aux Set.empty Set.empty x

    member __.ToString(n) = 
        let rec fmt depth (n: HashCons) =
            match n.Node with 
            | Leaf v ->
                if v.IsEmpty then "F"
                elif v = Range.Full then "T"
                else sprintf "[%d..%d]" v.Lo v.Hi
            | Node(v,l,r) ->
                if depth > 4 then "..." else
                sprintf "(%d => %s | %s)" v (fmt (depth+1) l) (fmt (depth+1) r)
        fmt 1 n


/// Helper bitwise operations for manipulating prefixes
/// so they can be stored in a compact integer, but manipulated
/// in dot notation x1.x2.x3.x4/s[lo..hi]

module Bitwise = 

    let inline shr x bits = if bits >= 32 then 0 else x >>> bits
    let inline shl x bits = if bits >= 32 then 0 else x <<< bits
    let inline isOne x i = (shr x (31-i)) &&& 1 = 1

    let fromDotted (x1,x2,x3,x4) = 
        (shl (x1 &&& 0x000000FF) 24) |||
        (shl (x2 &&& 0x000000FF) 16) |||
        (shl (x3 &&& 0x000000FF) 8) |||
        (shl (x4 &&& 0x000000FF) 0)

    let toDotted x = 
        let a = shr x 24
        let b = shr (shl x 8) 24
        let c = shr (shl x 16) 24
        let d = shr (shl x 24) 24
        (a, b, c, d)


/// Simple wrapper class for 32 bit prefixes with a convenience 
/// constructor that will read from dot notation string

type Prefix = class 
    val Bits: int32
    val Slash: int
    val Range: Range

    new(bits,s,r) = {Bits = bits; Slash = s; Range = r}
    new(a,b,c,d,s,r) = {Bits = Bitwise.fromDotted(a,b,c,d); Slash=s; Range = r}
    new(s: string) = 
        let a,b,c,d,s,x,y = Util.Scanf.sscanf "%d.%d.%d.%d/%d[%d..%d]" s
        {Bits = Bitwise.fromDotted(a,b,c,d); Slash=s; Range = Range(x,y)}
                            
    override v.ToString() = 
        let (a,b,c,d) = Bitwise.toDotted v.Bits
        let r = v.Range
        sprintf "%d.%d.%d.%d/%d[%d..%d]" a b c d v.Slash r.Lo r.Hi 
end


/// Predicate is just a wrapper around the root Bdd node

type Predicate = 
    Predicate of HashCons
 

/// Predicate builder class to encapsulate the Bdd data structure 
/// for manipulating predicates. Provides higher-level constructs 
/// like building a predicate directly from a prefix, or community value.

type PredicateBuilder() = 
    let bdd = BddBuilder(fun x y -> compare y x)

    let commMap = Dictionary<string,int>()
    let idxMap = Dictionary<int,string>()
    let mutable nextIdx = 32

    let truePrefix = Prefix("0.0.0.0/0[0..32]")

    let displayBinary ts fs = 
        let mutable res = ""
        for i in 0..31 do 
            if Set.contains i ts then res <- res + "1"
            elif Set.contains i fs then res <- res + "0"
            else res <- res + "x" 
        res

    /// Given a set of true and false variables, construct conjunction of prefixes
    let communities cts cfs = 
        let x = Seq.choose (fun x -> if x > 32 then Some x else None) cts 
        let y = Seq.choose (fun x -> if x > 32 then Some x else None) cfs
        (x,y)


    /// Given a set of true and false variables, construct a disjunction of prefixes (set)
    let prefixes pts pfs =
        let maxts = if Set.isEmpty pts then -1 else Set.maxElement pts
        let maxfs = if Set.isEmpty pfs then -1 else Set.maxElement pfs
        let largest = 1 + max maxts maxfs
        let rec aux i partialInt = 
            if i >= largest then Set.singleton partialInt
            else
                let newInt = partialInt ||| (Bitwise.shl 1 (31-i))
                if pts.Contains i then aux (i+1) newInt
                elif pfs.Contains i then aux (i+1) partialInt
                else Set.union (aux (i+1) newInt) (aux (i+1) partialInt)
        Seq.map (fun i -> Prefix(i, largest, Range(largest,32))) (aux 0 0)

    member __.Prefix (p: Prefix) =
        let mutable acc = bdd.Value p.Range
        let isExact = p.Range.Lo = p.Range.Hi
        for i in 0..31 do 
            if isExact || i < p.Slash then 
                if Bitwise.isOne p.Bits i 
                then acc <- bdd.And(acc, bdd.Var(i))
                else acc <- bdd.And(acc, bdd.Var(i) |> bdd.Not)
        Predicate(acc)
 
    member __.Community c = 
        let idx = 
            if commMap.ContainsKey(c) then commMap.[c]    
            else 
                commMap.[c] <- nextIdx 
                idxMap.[nextIdx] <- c
                nextIdx <- nextIdx + 1 
                nextIdx
        Predicate(bdd.Var(idx))

    member __.True = Predicate(bdd.True)
    member __.False = Predicate(bdd.False)
    member __.And(Predicate x, Predicate y) = Predicate(bdd.And(x,y))
    member __.Or(Predicate x, Predicate y) = Predicate(bdd.Or(x,y))
    member __.Not(Predicate x) = Predicate(bdd.Not(x))
    member __.Implies(Predicate x, Predicate y) = bdd.Implies(x,y)

    member __.ToString(Predicate p) = 
        bdd.ToString(p)

    member __.DoCrazy(Predicate p) : (seq<Prefix> * seq<string> * seq<string>) list =
        let acc = ref []
        let aux ts fs (r : Range) =
            if not r.IsEmpty then  
                let pts, cts = Set.partition (fun x -> x < 32) ts
                let pfs, cfs = Set.partition (fun x -> x < 32) fs
                let ps = prefixes pts pfs
                let cs = communities cts cfs
                printfn "%A" cs
                printfn "%A" ps
                printfn "%s" (displayBinary pts pfs)
        bdd.IterPath(aux, p)
        []