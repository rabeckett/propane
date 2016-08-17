module Route

open NUnit.Framework
open System
open System.Collections.Generic

/// Multi-terminal Binary decision diagram to represent predicates
/// over router prefixes, communities, topology locations etc.
/// Bdd nodes are hash consed for unicity, and have terminal 
/// nodes that represent a prefix length range. 
/// For example, the prefix predicate:  (1.2.3.0/8 ge 16 le 24)
/// would have bdd variables for each bit of the prefix, and 
/// a terminal node representing the length [16..24] 
let memoize f = 
  let unq = Dictionary()
  (fun x -> 
  if unq.ContainsKey(x) then unq.[x]
  else 
    let res = f x
    unq.[x] <- res
    res)

type Var = int

/// Simple range data structure representing 
/// a continuous range of valid prefix lengths 
/// between 0 and 32
type Range = 
  struct
    val Lo : int
    val Hi : int
    
    new(l, h) = 
      { Lo = l
        Hi = h }
    
    member this.IsEmpty = this.Lo < 0
    
    member r1.Union(r2 : Range) = 
      if r1.IsEmpty then r2
      elif r2.IsEmpty then r1
      else Range(min r1.Lo r2.Lo, max r1.Hi r2.Hi)
    
    member r1.Inter(r2 : Range) = 
      if r1.IsEmpty || r1.IsEmpty || r1.Lo > r2.Hi || r2.Lo > r1.Hi then Range.Empty
      else Range(max r1.Lo r2.Lo, min r1.Hi r2.Hi)
    
    member r.Negate() = 
      if r.IsEmpty then Range.Full
      else 
        match r.Lo, r.Hi with
        | 0, 32 -> Range.Empty
        | -1, -1 -> Range.Full
        | 0, x -> Range(x + 1, 32)
        | x, 32 -> Range(0, x - 1)
        | _ -> failwith "Invalid range negation"
    
    override r.ToString() = sprintf "%d..%d" r.Lo r.Hi
    static member Empty = Range(-1, -1)
    static member Full = Range(0, 32)
  end

/// Bdd nodes are hash consed to ensure maximal sharing.
/// This enables O(1) hashing, comparison, equality,
/// satisfiability and validity checks.
[<CustomComparison; CustomEquality>]
type HashCons = 
  struct
    val Id : int
    val Hash : int
    val Node : Node
    
    new(id, node) = 
      { Id = id
        Hash = hash node
        Node = node }
    
    override x.Equals(other) = 
      match other with
      | :? HashCons as y -> (x.Id = y.Id)
      | _ -> false
    
    override x.GetHashCode() = x.Hash
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
  let uniqueTab = Dictionary<Node, HashCons>()
  let mutable nextIdx = 1
  
  let mkNode (node : Node) : HashCons = 
    let b, value = uniqueTab.TryGetValue(node)
    if b then value
    else 
      let idx = nextIdx
      let hconsNode = HashCons(idx, node)
      uniqueTab.[node] <- hconsNode
      nextIdx <- nextIdx + 1
      hconsNode
  
  let leaf v = mkNode (Leaf v)
  
  let node (v, l : HashCons, r : HashCons) = 
    if l = r then l
    else mkNode (Node(v, l, r))
  
  let trueNode = leaf Range.Full
  let falseNode = leaf Range.Empty
  
  let rec negate (n : HashCons) = 
    match n.Node with
    | Leaf v -> leaf (v.Negate())
    | Node(v, l, r) -> node (v, negate l, negate r)
  
  let rec apply f (n1 : HashCons) (n2 : HashCons) = 
    match n1.Node, n2.Node with
    | Leaf v1, Leaf v2 -> leaf (f v1 v2)
    | Leaf _, Node(v, l, r) -> node (v, apply f l n1, apply f r n1)
    | Node(v, l, r), Leaf _ -> node (v, apply f l n2, apply f r n2)
    | Node(v1, l1, r1), Node(v2, l2, r2) -> 
      let v, (la, lb), (ra, rb) = 
        match order v1 v2 with
        | c when c = 0 -> v1, (l1, l2), (r1, r2)
        | c when c < 0 -> v1, (l1, n2), (r1, n2)
        | c -> v2, (n1, l2), (n1, r2)
      node (v, apply f la lb, apply f ra rb)
  
  let apply f = memoize (apply f)
  let opOr = apply (fun r1 r2 -> r1.Union(r2))
  let opAnd = apply (fun r1 r2 -> r1.Inter(r2))
  member __.NodeCount = nextIdx - 1
  member __.Value(v) = leaf v
  member __.Var(x) = node (x, trueNode, falseNode)
  member __.Ite(x, l, r) = node (x, l, r)
  member __.False = falseNode
  member __.True = trueNode
  member __.Or(n1, n2) = opOr n1 n2
  member __.And(n1, n2) = opAnd n1 n2
  member __.Not(n) = negate n
  member this.Implies(n1, n2) = this.And(n1, n2) = n1
  member __.ToString(n) = 
    let rec fmt depth (n : HashCons) = 
      match n.Node with
      | Leaf v -> 
        if v.IsEmpty then "F"
        elif v = Range.Full then "T"
        else sprintf "[%d..%d]" v.Lo v.Hi
      | Node(v, l, r) -> 
        if depth > 4 then "..."
        else sprintf "(%d => %s | %s)" v (fmt (depth + 1) l) (fmt (depth + 1) r)
    fmt 1 n

/// Helper bitwise operations for manipulating prefixes
/// so they can be stored in a compact integer, but
/// manipulated in dot notation x1.x2.x3.x4/s[lo..hi]
module Bitwise = 
  let inline shr x bits = 
    if bits >= 32 then 0
    else x >>> bits
  
  let inline shl x bits = 
    if bits >= 32 then 0
    else x <<< bits
  
  let inline get x i = (shr x (31 - i)) &&& 1 = 1
  let inline set x i = x ||| (shl 1 (31 - i))
  let fromDotted (x1, x2, x3, x4) = 
    (shl (x1 &&& 0x000000FF) 24) ||| (shl (x2 &&& 0x000000FF) 16) ||| (shl (x3 &&& 0x000000FF) 8) 
    ||| (shl (x4 &&& 0x000000FF) 0)
  
  let toDotted x = 
    let a = shr x 24
    let b = shr (shl x 8) 24
    let c = shr (shl x 16) 24
    let d = shr (shl x 24) 24
    (a, b, c, d)
  
  let toMask x = 
    assert (x >= 0 && x <= 32)
    let nBitsRight n = ~~~(shl 0xFFFFFFFF n)
    let x1 = nBitsRight (min 8 x)
    let x2 = nBitsRight (min 8 (max 0 (x - 8)))
    let x3 = nBitsRight (min 8 (max 0 (x - 16)))
    let x4 = nBitsRight (min 8 (max 0 (x - 24)))
    (x1, x2, x3, x4)

/// Simple wrapper class for 32 bit prefixes with a convenience 
/// constructor that will read from dot notation string.
[<StructuralEquality; StructuralComparison>]
type Prefix = 
  struct
    val Bits : int32
    val Slash : int
    val Range : Range
    val IsExact : bool
    val IsTemplate : bool
    val Name : string
    
    internal new(bits, s) = 
      { Bits = bits
        Slash = s
        Range = Range(s, s)
        IsExact = true
        IsTemplate = false
        Name = "" }
    
    internal new(bits, s, r) = 
      { Bits = bits
        Slash = s
        Range = r
        IsExact = false
        IsTemplate = false
        Name = "" }
    
    new(a, b, c, d, s) = 
      { Bits = Bitwise.fromDotted (a, b, c, d)
        Slash = s
        Range = Range(s, s)
        IsExact = true
        IsTemplate = false
        Name = "" }
    
    new(a, b, c, d, s, r) = 
      { Bits = Bitwise.fromDotted (a, b, c, d)
        Slash = s
        Range = r
        IsExact = false
        IsTemplate = false
        Name = "" }
    
    new(n) = 
      { Bits = 0
        Slash = 0
        Range = Range(-1, -1)
        IsExact = false
        IsTemplate = true
        Name = n }
    
    member v.Example(mask : bool) = 
      if v.IsTemplate then sprintf "%s" v.Name
      else if mask then 
        let (a, b, c, d) = Bitwise.toDotted v.Bits
        // let (m1, m2, m3, m4) = Bitwise.toMask v.Slash
        // sprintf "%d.%d.%d.%d mask %d.%d.%d.%d" a b c d m1 m2 m3 m4
        sprintf "%d.%d.%d.%d/%d" a b c d v.Slash
      else 
        let (a, b, c, d) = Bitwise.toDotted v.Bits
        sprintf "%d.%d.%d.%d/%d" a b c d v.Slash
    
    override v.ToString() = 
      if v.IsTemplate then sprintf "%s" v.Name
      else 
        let (a, b, c, d) = Bitwise.toDotted v.Bits
        let r = v.Range
        if v.IsExact then sprintf "%d.%d.%d.%d/%d" a b c d v.Slash
        else if r.Lo > v.Slash then sprintf "%d.%d.%d.%d/%d le %d ge %d" a b c d v.Slash r.Lo r.Hi
        else sprintf "%d.%d.%d.%d/%d le %d" a b c d v.Slash r.Hi
    
    static member True = Prefix(0, 0, 0, 0, 0, Range.Full)
    static member False = Prefix(0, 0, 0, 0, 0, Range.Empty)
  end

/// Predicate is either an abstract or concrete value
type Predicate = 
  | TemplatePred of Set<string>
  | ConcretePred of HashCons

/// Traffic classifier matches an individual prefix and positive / negative communities
type TrafficClassifier = 
  | TrafficClassifier of (Prefix * Set<string>)
  override x.ToString() = 
    let (TrafficClassifier(p, cts)) = x
    
    let comms = 
      Set.fold (fun acc ct -> 
        if acc <> "" then acc + "," + ct
        else ct) "" cts
    
    let comms = 
      Set.fold (fun acc ct -> 
        if acc <> "" then acc + ",!" + ct
        else "!" + ct) comms cts
    
    if cts.IsEmpty then sprintf "%s" (string p)
    else sprintf "%s, {%s}" (string p) comms

/// Predicate builder class to encapsulate the Bdd data structure 
/// for manipulating predicates. Provides higher-level constructs 
/// like building a predicate directly from a prefix, or community value.
/// And converting the Bdd encoding back to a human-readable format
type PredicateBuilder() = 
  // Not thread safe, so we use a lock
  let obj = new Object()
  // The order is important to easily reverse the encoding
  let bdd = BddBuilder(compare)
  let commToIdxMap = Dictionary<string, int>()
  let idxToCommMap = Dictionary<int, string>()
  let nextCommIdx = ref (-1)
  let topoToIdxMap = Dictionary<string, int>()
  let idxToTopoMap = Dictionary<int, string>()
  let nextTopoIdx = ref (-10001)
  let isPrefixVar x = (x >= 0 && x < 32)
  let isCommVar x = (x < 0 && x > -10000)
  let isTopoVar x = (x < -10000)
  
  /// Given a variable assignment, show prefix binary with don't care markers: 1000x0x1011
  let displayBinary ts fs (r : Range) = 
    let mutable res = ""
    for i in 0..31 do
      if Set.contains i ts then res <- res + "1"
      elif Set.contains i fs then res <- res + "0"
      else res <- res + "x"
    res + " " + (sprintf "[%d..%d]" r.Lo r.Hi)
  
  let filterTrueFalse f (map : Dictionary<_, _>) ts fs = 
    let aux vs = 
      vs
      |> Set.filter f
      |> Set.map (fun x -> map.[x])
    (aux ts, aux fs)
  
  /// Given a set of true and false variables, construct conjunction of prefixes
  let communities = filterTrueFalse isCommVar idxToCommMap
  
  /// Given a set of true and false variables, construct conjunction of topology locations
  let locations = filterTrueFalse isTopoVar idxToTopoMap
  
  /// Iterate over each path with a non-false terminal
  let iterPath f x = 
    let rec aux ts fs (n : HashCons) = 
      match n.Node with
      | Node(v, ({ Node = Leaf x } as l), r) when isCommVar v && x = Range.Full -> 
        aux (Set.add v ts) fs l
        aux ts fs r
      | Node(v, l, ({ Node = Leaf x } as r)) when isCommVar v && x = Range.Full -> 
        aux ts fs l
        aux ts (Set.add v fs) r
      | Node(v, l, r) -> 
        aux (Set.add v ts) fs l
        aux ts (Set.add v fs) r
      | Leaf v -> f ts fs v
    aux Set.empty Set.empty x
  
  /// Given a set of true and false variables, construct a disjunction of prefixes (set)
  let prefixes pts pfs (r : Range) = 
    let maxts = 
      if Set.isEmpty pts then -1
      else Set.maxElement pts
    
    let maxfs = 
      if Set.isEmpty pfs then -1
      else Set.maxElement pfs
    
    let largest = 1 + max maxts maxfs
    
    let rec aux i partialInt = 
      if i >= largest then Set.singleton partialInt
      else 
        let newInt = Bitwise.set partialInt i
        if pts.Contains i then aux (i + 1) newInt
        elif pfs.Contains i then aux (i + 1) partialInt
        else Set.union (aux (i + 1) newInt) (aux (i + 1) partialInt)
    
    let isExact = largest = 32 && r.Lo = r.Hi
    if isExact then Seq.map (fun i -> Prefix(i, r.Hi)) (aux 0 0)
    else 
      let r = Range(max r.Lo largest, r.Hi)
      Seq.map (fun i -> Prefix(i, largest, r)) (aux 0 0)
  
  let addForString (map : Dictionary<_, _>) (idxMap : Dictionary<_, _>) v idxVar = 
    let idx = 
      let b, value = map.TryGetValue(v)
      if b then value
      else 
        map.[v] <- !idxVar
        idxMap.[!idxVar] <- v
        let res = !idxVar
        idxVar := !idxVar - 1
        res
    bdd.Var(idx)
  
  /// Return the bdd representing a prefix x1.x2.x3.x4/s[lo..hi]
  member __.Prefix(p : Prefix) = 
    lock obj (fun () -> 
      let mutable acc = bdd.Value p.Range
      for i in 0..31 do
        if p.IsExact || i < p.Slash then 
          if Bitwise.get p.Bits i then acc <- bdd.And(acc, bdd.Var(i))
          else acc <- bdd.And(acc, bdd.Var(i) |> bdd.Not)
      acc)
  
  /// Return the bdd representing a match on the community c
  member __.Community c = lock obj (fun () -> addForString commToIdxMap idxToCommMap c nextCommIdx)
  
  /// Return the bdd representing a match on the community c
  member __.Location l = lock obj (fun () -> addForString topoToIdxMap idxToTopoMap l nextTopoIdx)
  
  /// The predicate matching any route announcement
  member __.True = bdd.True
  
  /// The predicate matching no route announcements
  member __.False = bdd.False
  
  /// The predicate matching both x and y
  member __.And(x, y) = lock obj (fun () -> bdd.And(x, y))
  
  /// The predicate matching either x or y
  member __.Or(x, y) = lock obj (fun () -> bdd.Or(x, y))
  
  /// The predicate not matching x
  member __.Not(x) = lock obj (fun () -> bdd.Not(x))
  
  /// Check if one predicate implies the other
  member __.Implies(x, y) = lock obj (fun () -> bdd.Implies(x, y))
  
  /// Convert the Bdd representation of a predicate to a prioritized list of matches
  member __.TrafficClassifiers(p) : TrafficClassifier list = 
    let acc = ref []
    
    let aux ts fs (r : Range) = 
      if not r.IsEmpty then 
        let pts, ots = Set.partition isPrefixVar ts
        let pfs, ofs = Set.partition isPrefixVar fs
        let cts, tts = Set.partition isCommVar ots
        let cfs, tfs = Set.partition isCommVar ofs
        let ps = prefixes pts pfs r
        let (cts, _) = communities cts cfs
        let (tts, _) = locations tts tfs
        for p in ps do
          acc := TrafficClassifier(p, cts) :: !acc
    iterPath aux p
    List.rev !acc
  
  /// Pick out a single representative assignment
  member x.Example(p) : TrafficClassifier = 
    let tcs = x.TrafficClassifiers(p)
    tcs.Head
  
  /// Output predicate as traffic classifiers representation
  member x.ToString(p) = 
    let tcs = x.TrafficClassifiers(p)
    let ex = tcs.Head
    if tcs.Length = 1 then sprintf "%s" (string ex)
    else sprintf "%s or ..." (string ex)

/// Globally unique predicate builder to ensure hash consing uniqueness
let pb = PredicateBuilder()

/// Create a template variable
let inline templateVar r = TemplatePred(Set.singleton r)

/// Check if a predicate is abstract
let inline isTemplate x = 
  match x with
  | TemplatePred _ -> true
  | ConcretePred _ -> false

/// True representation
let top = ConcretePred(pb.True)

/// False representation
let bot = ConcretePred(pb.False)

/// Prefix representation
let inline prefix p = ConcretePred(pb.Prefix p)

/// Community representation
let inline community c = ConcretePred(pb.Community c)

/// Topology location representation
let inline location l = ConcretePred(pb.Location l)

/// Get concrete value
let inline getConcrete (x : Predicate) = 
  match x with
  | TemplatePred _ -> failwith "Invariant violation: getConcrete"
  | ConcretePred p -> p

/// Conjunction of two predicates
let conj (x : Predicate) (y : Predicate) = 
  match x, y with
  | TemplatePred r1, TemplatePred r2 -> 
    let n = Set.intersect r1 r2
    if Set.isEmpty n then ConcretePred(pb.False)
    else TemplatePred n
  | TemplatePred _, ConcretePred v -> 
    if v = pb.True then x
    else ConcretePred(pb.False)
  | ConcretePred v, TemplatePred _ -> 
    if v = pb.True then y
    else ConcretePred(pb.False)
  | ConcretePred x, ConcretePred y -> ConcretePred(pb.And(x, y))

/// Disjunction of two predicates
let disj (x : Predicate) (y : Predicate) = 
  match x, y with
  | TemplatePred r1, TemplatePred r2 -> TemplatePred(Set.union r1 r2)
  | TemplatePred _, ConcretePred v -> 
    if v = pb.True then ConcretePred(pb.True)
    else x
  | ConcretePred v, TemplatePred _ -> 
    if v = pb.True then ConcretePred(pb.True)
    else y
  | ConcretePred x, ConcretePred y -> ConcretePred(pb.Or(x, y))

/// Test for prefix superset
let mightApplyTo (x : Prefix) (y : Prefix) = 
  if x.IsTemplate || y.IsTemplate then true
  else pb.Implies(pb.Prefix y, pb.Prefix x)

/// Traffic Classifiers of a predicate
let trafficClassifiers (x : Predicate) = 
  match x with
  | ConcretePred p -> pb.TrafficClassifiers p
  | TemplatePred rs -> Set.fold (fun acc r -> TrafficClassifier(Prefix(r), Set.empty) :: acc) [] rs

// Simplify printing to a string 
let toString (x : Predicate) = 
  let aux acc r = 
    if acc = "" then r
    else acc + " or " + r
  match x with
  | TemplatePred rs -> Set.fold aux "" rs
  | ConcretePred p -> pb.ToString(p)

/// Unit tests for the Bdd data structure 
/// Ensures basic logic properties hold and 
/// that Bdd node uniqueness is preserved
[<TestFixture>]
type TestBdd() = 
  let bb = BddBuilder(compare)
  
  [<Test>]
  member __.HashConsUnicity() = 
    let bb = BddBuilder(compare)
    // Don't create separate nodes for 1
    let x = bb.Var(1)
    let y = bb.Var(1)
    let res = bb.Ite(2, x, y)
    // Don't create node for 2
    Assert.AreEqual(3, bb.NodeCount)
  
  [<Test>]
  member __.Identity() = 
    Assert.AreEqual(bb.True, bb.Or(bb.True, bb.False))
    Assert.AreEqual(bb.True, bb.Or(bb.False, bb.True))
    Assert.AreEqual(bb.True, bb.And(bb.True, bb.True))
    Assert.AreEqual(bb.False, bb.Or(bb.False, bb.False))
    Assert.AreEqual(bb.False, bb.And(bb.False, bb.True))
    Assert.AreEqual(bb.False, bb.And(bb.True, bb.False))
  
  [<Test>]
  member __.Commutativity() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    Assert.AreEqual(bb.And(x, y), bb.And(y, x))
    Assert.AreEqual(bb.Or(x, y), bb.Or(y, x))
  
  [<Test>]
  member __.Associativity() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    let z = bb.Var(3)
    Assert.AreEqual(bb.And(x, bb.And(y, z)), bb.And(bb.And(x, y), z))
    Assert.AreEqual(bb.Or(x, bb.Or(y, z)), bb.Or(bb.Or(x, y), z))
  
  [<Test>]
  member __.DisjunctionIdentity() = 
    let x = bb.Var(1)
    Assert.AreEqual(bb.True, bb.Or(bb.True, x))
    Assert.AreEqual(bb.True, bb.Or(x, bb.True))
    Assert.AreEqual(x, bb.Or(bb.False, x))
    Assert.AreEqual(x, bb.Or(x, bb.False))
  
  [<Test>]
  member __.DisjunctionIdempotent() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    let x = bb.Var(1)
    let z = bb.Or(x, y)
    Assert.AreEqual(x, bb.Or(x, x))
    Assert.AreEqual(z, bb.Or(x, z))
    Assert.AreEqual(z, bb.Or(y, z))
  
  [<Test>]
  member __.ConjunctionIdentity() = 
    let x = bb.Var(1)
    Assert.AreEqual(bb.False, bb.And(bb.False, x))
    Assert.AreEqual(bb.False, bb.And(x, bb.False))
    Assert.AreEqual(x, bb.And(bb.True, x))
    Assert.AreEqual(x, bb.And(x, bb.True))
  
  [<Test>]
  member __.ConjunctionIdempotent() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    let x = bb.Var(1)
    let z = bb.And(x, y)
    Assert.AreEqual(x, bb.And(x, x))
    Assert.AreEqual(z, bb.And(x, z))
    Assert.AreEqual(z, bb.And(y, z))
  
  [<Test>]
  member __.Distributivity() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    let z = bb.Var(3)
    Assert.AreEqual(bb.And(x, bb.Or(y, z)), bb.Or(bb.And(x, y), bb.And(x, z)))
    Assert.AreEqual(bb.And(bb.Or(y, z), x), bb.Or(bb.And(y, x), bb.And(z, x)))
    Assert.AreEqual(bb.Or(x, bb.And(y, z)), bb.And(bb.Or(x, y), bb.Or(x, z)))
    Assert.AreEqual(bb.Or(bb.And(y, z), x), bb.And(bb.Or(y, x), bb.Or(z, x)))
  
  [<Test>]
  member __.DoubleNegation() = 
    let x = bb.Var(1)
    Assert.AreEqual(x, bb.Not(bb.Not x))
  
  [<Test>]
  member __.Implies() = 
    let x = bb.Var(1)
    let y = bb.Var(2)
    let z = bb.Or(x, y)
    Assert.IsTrue(bb.Implies(x, x))
    Assert.IsTrue(bb.Implies(y, z))
    Assert.IsTrue(bb.Implies(x, z))
  
  [<Test>]
  member __.RangeOps() = 
    let r1 = Range(0, 20)
    let r2 = Range(10, 30)
    let r3 = Range(10, 20)
    let r4 = Range(0, 30)
    let r5 = Range(21, 32)
    let x = bb.Ite(1, bb.Value r1, bb.Value r2)
    Assert.AreEqual(x, bb.And(x, x))
    Assert.AreEqual(x, bb.Or(x, x))
    Assert.AreEqual(bb.Value r3, bb.And(bb.Value r1, bb.Value r2))
    Assert.AreEqual(bb.Value r4, bb.Or(bb.Value r1, bb.Value r2))
    Assert.AreEqual(bb.Value r5, bb.Not(bb.Value r1))

/// Unit tests for the Predicate data structure 
/// Checks that the Bdd encoding is valid, and can 
/// be reversed to a prioritized list or rules
[<TestFixture>]
type TestPredicate() = 
  let pb = PredicateBuilder()
  
  let equalRules (x : _ list) (y : _ list) = 
    Assert.AreEqual(x.Length, y.Length)
    for vx in x do
      Assert.IsTrue(List.contains vx y)
  
  [<Test>]
  member __.Identities() = 
    let p1 = Prefix(0, 0, 0, 1, 32, Range(32, 32))
    let p = pb.Prefix p1
    Assert.AreEqual(pb.True, pb.Or(pb.True, p))
    Assert.AreEqual(pb.True, pb.Or(p, pb.True))
    Assert.AreEqual(p, pb.And(p, pb.True))
    Assert.AreEqual(p, pb.And(pb.True, p))
    Assert.AreEqual(pb.False, pb.And(pb.False, p))
    Assert.AreEqual(pb.False, pb.And(p, pb.False))
    Assert.AreEqual(p, pb.Or(p, pb.False))
    Assert.AreEqual(p, pb.Or(pb.False, p))
  
  [<Test>]
  member __.ExactAndRange() = 
    let p1 = Prefix(0, 0, 0, 0, 32)
    let p2 = Prefix(0, 0, 0, 1, 32)
    let p3 = Prefix(0, 0, 0, 0, 31, Range(32, 32))
    let x = pb.Prefix p1
    let y = pb.Prefix p2
    let z = pb.Prefix p3
    Assert.AreEqual(z, pb.Or(x, y))
  
  [<Test>]
  member __.SupersetPrefix() = 
    let p1 = Prefix(0, 1, 0, 2, 24, Range(24, 32))
    let p2 = Prefix(0, 1, 0, 1, 16, Range(16, 32))
    let x = pb.Prefix p1
    let y = pb.Prefix p2
    Assert.AreEqual(x, pb.And(x, y))
    Assert.AreEqual(y, pb.Or(x, y))
    Assert.IsTrue(pb.Implies(x, y))
    Assert.IsTrue(pb.True = pb.Or(pb.Not x, y))
  
  [<Test>]
  member __.ExactNotTheSameAsRange() = 
    let p1 = Prefix(0, 0, 0, 1, 24)
    let p2 = Prefix(0, 0, 0, 1, 24, Range(24, 24))
    let x = pb.Prefix p1
    let y = pb.Prefix p2
    Assert.AreNotEqual(x, y)
  
  [<Test>]
  member __.LargerSlash() = 
    let p1 = Prefix(0, 0, 0, 1, 24, Range(24, 30))
    let p2 = Prefix(0, 0, 1, 1, 24, Range(24, 30))
    let p3 = Prefix(0, 0, 0, 0, 23, Range(24, 30))
    let x = pb.Prefix p1
    let y = pb.Prefix p2
    let z = pb.Prefix p3
    Assert.AreEqual(z, pb.Or(x, y))
  
  [<Test>]
  member __.EmptyConjunction() = 
    let p1 = Prefix(0, 0, 0, 1, 24, Range(24, 30))
    let p2 = Prefix(0, 0, 1, 1, 24, Range(24, 30))
    let x = pb.Prefix p1
    let y = pb.Prefix p2
    Assert.AreEqual(pb.False, pb.And(x, y))
  
  [<Test>]
  member __.ExpandUnknownBits() = 
    let p1 = Prefix(0, 0, 0, 1, 32)
    let p2 = Prefix(0, 0, 0, 3, 32)
    let pred = pb.Or(pb.Prefix p1, pb.Prefix p2)
    let vs = pb.TrafficClassifiers(pred)
    equalRules vs [ TrafficClassifier(p1, Set.empty)
                    TrafficClassifier(p2, Set.empty) ]
  
  [<Test>]
  member __.AvoidExtraNegation() = 
    let p1 = Prefix(0, 0, 0, 1, 32)
    let p2 = Prefix(0, 0, 0, 3, 32)
    let pred = pb.Or(pb.Prefix p1, pb.Prefix p2)
    let pred = pb.Or(pred, pb.Community "A")
    let vs = pb.TrafficClassifiers(pred)
    equalRules vs [ TrafficClassifier(p1, Set.empty)
                    TrafficClassifier(p2, Set.empty)
                    TrafficClassifier(Prefix.True, Set.singleton "A") ]
  
  [<Test>]
  member __.RecoverExact() = 
    let p1 = Prefix(0, 0, 1, 1, 24)
    let x = pb.Prefix p1
    let vs = pb.TrafficClassifiers(x)
    equalRules vs [ TrafficClassifier(p1, Set.empty) ]
  
  [<Test>]
  member __.MultipleCommunitiesOr() = 
    let p1 = Prefix(0, 0, 0, 1, 32)
    let p2 = Prefix(0, 0, 0, 3, 32)
    let pred = pb.Or(pb.Prefix p1, pb.Prefix p2)
    let pred = pb.Or(pred, pb.Community "A")
    let pred = pb.Or(pred, pb.Community "B")
    let vs = pb.TrafficClassifiers(pred)
    equalRules vs [ TrafficClassifier(p1, Set.empty)
                    TrafficClassifier(p2, Set.empty)
                    TrafficClassifier(Prefix.True, Set.singleton "A")
                    TrafficClassifier(Prefix.True, Set.singleton "B") ]
  
  [<Test>]
  member __.MultipleCommunitiesAnd() = 
    let p1 = Prefix(0, 0, 0, 1, 32)
    let p2 = Prefix(0, 0, 0, 3, 32)
    let c1 = pb.Community "A"
    let c2 = pb.Community "B"
    let x = pb.Or(pb.Prefix p1, pb.Prefix p2)
    let y = pb.And(c1, c2)
    let pred = pb.And(x, y)
    let vs = pb.TrafficClassifiers(pred)
    equalRules vs [ TrafficClassifier(p1, Set.ofList [ "A"; "B" ])
                    TrafficClassifier(p2, Set.ofList [ "A"; "B" ]) ]
  
  [<Test>]
  member __.CommunityRangeKnowledge() = 
    let c1 = pb.Community "A"
    let c2 = pb.Community "B"
    let c3 = pb.Community "C"
    let x = pb.Or(c1, pb.Or(c2, c3))
    let knowledge = pb.And(pb.Not c1, pb.And(pb.Not c2, pb.Not c3))
    Assert.AreEqual(pb.True, pb.Or(x, knowledge))
  
  [<Test>]
  member __.TopologyRangeKnowledge() = 
    let t1 = pb.Location "X"
    let t2 = pb.Location "Y"
    let t3 = pb.Location "Z"
    let x = pb.Or(t1, pb.Or(t2, t3))
    let knowledge = pb.And(pb.Not t1, pb.And(pb.Not t2, pb.Not t3))
    Assert.AreEqual(pb.True, pb.Or(x, knowledge))
  
  [<Test>]
  member __.BothRangeKnowledge() = 
    let c1 = pb.Community "A"
    let c2 = pb.Community "B"
    let c3 = pb.Community "C"
    let t1 = pb.Location "X"
    let t2 = pb.Location "Y"
    let t3 = pb.Location "Z"
    let x = pb.Or(c1, pb.Or(c2, c3))
    let y = pb.Or(t1, pb.Or(t2, t3))
    let knowledgec = pb.And(pb.Not c1, pb.And(pb.Not c2, pb.Not c3))
    let knowledget = pb.And(pb.Not t1, pb.And(pb.Not t2, pb.Not t3))
    Assert.AreEqual(pb.True, pb.Or(x, pb.Or(knowledgec, knowledget)))