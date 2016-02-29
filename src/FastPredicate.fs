module FastPredicate

open System.Collections.Generic

/// Binary decision diagram
/// BddNodes are hash consed for unicity

let memoize f =
    let unq = Dictionary()
    (fun x -> 
        if unq.ContainsKey(x) then unq.[x]
        else 
            let res = f x 
            unq.[x] <- res ;res)

type Var = string
type BddIndex = int
type Bdd = Bdd of BddIndex


type BddNode = struct 
    val Var: Var
    val Left: BddIndex
    val Right: BddIndex
    new(v,l,r) = {Var = v; Left = l; Right = r}
end 

type BddBuilder(order : Var -> Var -> int) =

    let uniqueTab = new Dictionary<BddNode,BddIndex>() 
    let nodeTab = new Dictionary<BddIndex,BddNode>()

    let mutable nextIdx = 2 
    let trueIdx = 1
    let falseIdx = -1

    let trueNode = BddNode("",trueIdx,trueIdx)
    let falseNode = BddNode("",falseIdx,falseIdx)

    // Map indexes to nodes. Negative indexes go to their negation. The special // indexes -1 and 1 go to special true/false nodes.
    let idxToNode(idx) =
        if idx = trueIdx then trueNode
        elif idx = falseIdx then falseNode
        elif idx > 0 then nodeTab.[idx]
        else 
            let n = nodeTab.[-idx]
            BddNode(n.Var,-n.Left,-n.Right)

    // Map nodes to indexes. Add an entry to the table if needed.
    let nodeToUniqueIdx(node) =
        if uniqueTab.ContainsKey(node) then uniqueTab.[node] else
        let idx = nextIdx 
        uniqueTab.[node] <- idx 
        nodeTab.[idx] <- node 
        nextIdx <- nextIdx + 1 
        idx

    // Get the canonical index for a node. Preserve the invariant that the 
    // left-hand node of a conditional is always a positive node
    let mkNode(v:Var,l:BddIndex,r:BddIndex) =
        if l = r then l
        elif l >= 0 then nodeToUniqueIdx(BddNode(v,l,r) ) 
        else -nodeToUniqueIdx(BddNode(v,-l,-r))

    let rec mkAnd(m1,m2) =
        if m1 = falseIdx || m2 = falseIdx then falseIdx
        elif m1 = trueIdx then m2
        elif m2 = trueIdx then m1
        else 
            let n1 = idxToNode(m1)
            let n2 = idxToNode(m2)
            let x, l1, r1 = n1.Var, n1.Left, n1.Right
            let y, l2, r2 = n2.Var, n2.Left, n2.Right
            let v,(la,lb),(ra,rb) = 
                match order x y with 
                | c when c = 0 -> x,(l1,l2),(r1,r2)
                | c when c < 0 -> x,(l1,m2),(r1,m2)
                | c            -> y,(m1,l2),(m1,r2)
            mkNode(v, mkAnd(la,lb), mkAnd(ra,rb))

    let mkAnd = memoize mkAnd

    member __.False = Bdd falseIdx
    member __.True = Bdd trueIdx
    member __.And(Bdd m1, Bdd m2) = Bdd(mkAnd(m1,m2))
    member __.Not(Bdd m) = Bdd(-m)
    member __.Or(Bdd m1, Bdd m2) = Bdd(-mkAnd(-m1, -m2))
    member __.Var(nm) = Bdd(mkNode(nm,trueIdx,falseIdx))
    member __.Implies(Bdd m1, Bdd m2) = mkAnd(m1, -m2) = falseIdx
    member __.NodeCount = nextIdx

    // iterate over each path to a leaf node
    member __.IterPath(f, Bdd x) = 
        let rec aux path idx =
            if idx = trueIdx then f path 
            elif idx = falseIdx then ()
            else
                let n = idxToNode(idx)
                aux (n.Var :: path) n.Left 
                aux path n.Right
        aux [] x

    member __.ToString(Bdd idx) = 
        let rec fmt depth idx =
            if depth > 4 then "..." else
            let n = idxToNode(idx)
            if n.Var = "" then if n.Left = trueIdx then "T" else "F"
            else sprintf "(%s => %s | %s)" n.Var (fmt (depth+1) n.Left) (fmt (depth+1) n.Right)
        fmt 1 idx


type Prefix = struct 
    val X1: uint32
    val X2: uint32 
    val X3: uint32
    val X4: uint32
    val Slash: uint32
    new(a,b,c,d,e) = {X1=a; X2=b; X3=c; X4=d; Slash=e}
    override v.ToString() = sprintf "%d.%d.%d.%d/%d" v.X1 v.X2 v.X3 v.X4 v.Slash 
end

type Predicate = Predicate of Bdd
 
type PredicateBuilder(comms: Set<string>) = 
    let builder = BddBuilder(compare)

    let addVars(onBits: bool [], onComms: Set<string>) =
        let bdd = ref builder.True
        Array.iteri (fun i b ->
            if b then 
                let var = "p" + string i in 
                bdd := builder.And(!bdd, builder.Var(var)) ) onBits
        Set.iter (fun s -> 
            let var = "c" + s
            bdd := builder.And(!bdd, builder.Var(var)) ) onComms
        Predicate(!bdd)

    let shr x bits = 
        if bits >= 32 then 0u else x >>> bits

    let shl x bits =
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

    let arrayOfPrefix (p: Prefix) = 
        Array.init 32 (fun i -> 
            if i <= int p.Slash then 
                let (m,r) = i/8, i%8
                if m = 0 then isOne p.X1 (31-r)
                elif m = 1 then isOne p.X2 (31-r)
                elif m = 2 then isOne p.X3 (31-r)
                else isOne p.X4 r
            else false)

    member __.Prefix (p: Prefix) =
        let onBits = arrayOfPrefix p
        addVars(onBits, Set.empty)

    member __.Community c = 
        if not (Set.contains c comms) then 
            failwith (sprintf "Invalid community value: %s" c)
        addVars(Array.empty, Set.singleton c)

    member __.True = Predicate(builder.True)
    member __.False = Predicate(builder.False)
    member __.And(Predicate x, Predicate y) = Predicate(builder.And(x,y))
    member __.Or(Predicate x, Predicate y) = Predicate(builder.Or(x,y))
    member __.Not(Predicate x) = Predicate(builder.Not(x))
    member __.Implies(Predicate x, Predicate y) = builder.Implies(x,y)

    member __.ToString(Predicate p) = 
        builder.ToString(p)

    member __.DoCrazy(Predicate p) = 
        let f = fun vars -> printfn "path: %A" vars
        builder.IterPath(f, p)