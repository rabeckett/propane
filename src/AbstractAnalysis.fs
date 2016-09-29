module AbstractAnalysis

open CGraph
open Core.Printf
open System.Collections.Generic
open System.Text
open Util.Format

module Z3 = 
   type Result = 
      | Error of string
      | Minimized of int
      | Sat
      | Unsat
   
   let run (input : string) : Result = 
      let cmds = [ "z3"; "-smt2"; "-in" ]
      let input = input + "(check-sat)"
      let output = Util.Command.run cmds (Some input)
      match output with
      | None -> failwith "invalid command"
      | Some txt -> 
         if txt.StartsWith("sat") then 
            let lines = Util.String.toLines txt
            if lines.Length >= 3 then 
               let l = lines.[2]
               if l.Length >= 5 then 
                  try 
                     Minimized(int (l.Substring(4, l.Length - 1 - 4)))
                  with _ -> Sat
               else Sat
            else Sat
         else if txt.StartsWith("unsat") then Unsat
         else Error txt

let inline writeDeclaration sb name = bprintf sb "(declare-const %s Int)\n" name

let inline writeFormula sb form = 
   if form <> "true" then bprintf sb "(assert %s)\n" form

let baseEncoding (ti : Topology.TopoInfo) = 
   let sb = StringBuilder()
   // Base constraints
   for kv in ti.NodeLabels do
      let n = kv.Value
      writeDeclaration sb n
   for kv in ti.EdgeLabels do
      let n = kv.Value
      writeDeclaration sb n
   for n in ti.PodLabels do 
      writeDeclaration sb n
   for kv in ti.NodeLabels do
      let n = kv.Value
      bprintf sb "(assert (>= %s 0))\n" n
   for kv in ti.EdgeLabels do
      let n = kv.Value
      bprintf sb "(assert (>= %s 0))\n" n
   for n in ti.PodLabels do 
      bprintf sb "(assert (>= %s 0))\n" n
   for c in ti.Constraints do
      writeFormula sb c
   // Edge multiplicities do not exceed the node multiplicity
   let agi = ti.AbstractGraphInfo
   let topoA = agi.Graph
   let abs = ti.Abstraction
   for (n1, n2) in Topology.edges topoA do
      let a1 = Topology.routerMap agi.RouterMap n1.Loc
      let a2 = Topology.routerMap agi.RouterMap n2.Loc
      let labelN = ti.NodeLabels.[a2]
      let labelE1 = ti.EdgeLabels.[(a1, a2)]
      bprintf sb "(assert (>= %s %s))\n" labelN labelE1
   sb

let inline getNodeConstraintName (ti : Topology.TopoInfo) v = 
   let name = Topology.router v.Node.Loc ti
   ti.NodeLabels.[name]

let inline getEdgeConstraintNames (ti : Topology.TopoInfo) v u = 
   let namev = Topology.router v.Node.Loc ti
   let nameu = Topology.router u.Node.Loc ti
   let e1 = ti.EdgeLabels.[(namev, nameu)]
   let e2 = ti.EdgeLabels.[(nameu, namev)]
   (e1, e2)

let minimumEncoding e = 
   let x = sprintf "(declare-const m Int)\n"
   let y = sprintf "(assert (= m %s))\n" e
   let z = sprintf "(minimize m)\n"
   x + y + z

let isUnsat enc a = 
   let constr = sprintf "%s\n" a
   let check = enc + constr
   match Z3.run check with
   | Z3.Unsat -> true
   | Z3.Sat -> false
   | Z3.Error s -> failwith (sprintf "Invalid command: %s" s)
   | Z3.Minimized k -> failwith (sprintf "Invalid result: minimized")

let findMin enc x = 
   let check = enc + (minimumEncoding x)
   match Z3.run check with
   | Z3.Minimized k -> Some k
   | _ -> None

let checkIsGraphHomomorphism (ti : Topology.TopoInfo) = 
   let topoC = ti.ConcreteGraphInfo.Graph
   let topoA = ti.AbstractGraphInfo.Graph
   let abstractEdges = HashSet()
   for (n1, n2) in Topology.edges topoA do
      let a1 = Topology.router n1.Loc ti
      let a2 = Topology.router n2.Loc ti
      abstractEdges.Add(a1, a2) |> ignore
      abstractEdges.Add(a2, a1) |> ignore
   let abs = ti.Abstraction
   for (n1, n2) in Topology.edges topoC do
      let c1 = Topology.routerMap ti.ConcreteGraphInfo.RouterMap n1.Loc
      let c2 = Topology.routerMap ti.ConcreteGraphInfo.RouterMap n2.Loc
      match abs.TryFind c1, abs.TryFind c2 with
      | Some a1, Some a2 -> 
         if not <| abstractEdges.Contains(a1, a2) then 
            let msg = 
               sprintf "The concrete topology is not valid for the given abstract topology. " 
               + sprintf "For example, the concrete edge: (%s,%s) does not have a corresponding " c1 
                    c2 + sprintf "abstract edge between: (%s,%s)" a1 a2
            error msg
      | None, _ -> error (sprintf "Missing group definition for concrete node: %s" c1)
      | _, None -> error (sprintf "Missing group definition for concrete node: %s" c2)

let checkHasValidConstraints (ti : Topology.TopoInfo) = 
   let cgi = ti.ConcreteGraphInfo
   let agi = ti.AbstractGraphInfo
   let (topoC, topoA) = cgi.Graph, agi.Graph
   let abs = ti.Abstraction
   // Count the actual node + edge multiplicities
   let mutable nodeCounts = Map.empty
   let mutable edgeCounts = Map.empty
   for n1 in Topology.vertices topoC do
      let c1 = Topology.routerMap cgi.RouterMap n1.Loc
      let a1 = abs.[c1]
      nodeCounts <- Util.Map.adjust a1 0 ((+) 1) nodeCounts
      let mutable countByGroup = Map.empty
      for n2 in Topology.neighbors cgi.Graph n1 do
         let c2 = Topology.routerMap cgi.RouterMap n2.Loc
         let a2 = abs.[c2]
         countByGroup <- Util.Map.adjust a2 0 ((+) 1) countByGroup
      for kv in countByGroup do
         let a2 = kv.Key
         let x = kv.Value
         match Map.tryFind (a1, a2) edgeCounts with
         | None -> edgeCounts <- Map.add (a1, a2) x edgeCounts
         | Some y -> edgeCounts <- Map.add (a1, a2) (min x y) edgeCounts
   // Add exact value constraints
   let sb = baseEncoding ti
   for kv in nodeCounts do
      let label = ti.NodeLabels.[kv.Key]
      bprintf sb "(assert (= %s %d))\n" label kv.Value
   for kv in edgeCounts do
      let label = ti.EdgeLabels.[kv.Key]
      bprintf sb "(assert (>= %s %d))\n" label kv.Value
   if isUnsat (string sb) "" then 
      let msg = 
         sprintf 
            "Invalid concrete topology. The topology did not satisfy the abstract constraints provided"
      error msg

let checkWellformedTopology (ti : Topology.TopoInfo) = 
   match ti.Kind with
   | Topology.Concrete | Topology.Template -> ()
   | Topology.Abstract -> 
      checkIsGraphHomomorphism ti
      checkHasValidConstraints ti

(*
let debugLearned (learned : Dictionary<_, _>) ti = 
   let inline pretty x = 
      if x = -1 then "UNREACH"
      else string x
   printfn "========================="
   for kv in learned do
      if CGraph.isRealNode kv.Key then 
         let (s, a) = kv.Value
         printfn "%s --> (%s,%s)" (Topology.router kv.Key.Node.Loc ti) (pretty s) (pretty a)
   printfn "========================="

let reachability2 (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : int option = 
   printfn "Got call for src: %s" (string src)
   // Capture the base constraints
   let enc = string (baseEncoding ti)
   // Represent UNREACH with -1
   let learned = Dictionary()
   for n in cg.Graph.Vertices do
      if n = src then learned.[n] <- (1, -1)
      else learned.[n] <- (-1, -1)
   // Run to a fixed point using inference rules
   printfn "adding initial node: %s" src.Node.Loc
   let changed = ref (Set.singleton src)
   let first = ref true
   
   let inline pop() = 
      let ret = Set.minElement !changed
      changed := Set.remove ret !changed
      ret
   
   let inline push x = 
      if isInside x then changed := Set.add x !changed
   
   while (!changed).Count > 0 do
      debugLearned learned ti
      let v = pop()
      printfn "looking at node: %s" v.Node.Loc
      let (kS, kA) = learned.[v]
      for u in CGraph.neighbors cg v |> Seq.filter CGraph.isRealNode do
         let (kS', kA') = learned.[u]
         printfn "  looking at neighbor: %s" u.Node.Loc
         let m = getNodeConstraintName ti v
         let n = getNodeConstraintName ti u
         let (e1, e2) = getEdgeConstraintNames ti v u
         // for debugging
         let namev = Topology.router v.Node.Loc ti
         let nameu = Topology.router u.Node.Loc ti
         printfn "  for nodes: (%s,%s)" namev nameu
         printfn "  got labels: (%s,%s,%s,%s)" m n e1 e2
         // if we have an A label, then we have an S label
         if kS < kA then learned.[v] <- (kA, kA)
         // lookup existing values of k
         // if third rule applies
         if kS > 0 || kA > 0 then 
            printfn "  checking rule 1 - S"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat enc a then 
               printfn "   Rule 1-1"
               match findMin enc e1 with
               | Some k -> 
                  printfn "   found min: %d" k
                  let m = 
                     if !first then k
                     else min k (max kS kA)
                  if m > kS' then 
                     learned.[u] <- (m, kA)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
         // if second rule applies
         if kS > 0 || kA > 0 then 
            printfn "  checking rule 2 - S"
            let a = sprintf "(assert (not (= %s %s)))" e1 n
            if isUnsat enc a then 
               printfn "   Rule 2-1"
               match findMin enc n with
               | Some k -> 
                  printfn "   found min: %d" k
                  let m = 
                     if !first then k
                     else min k (max kS kA)
                  if m > kA' then 
                     learned.[u] <- (kS, m)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
         // if first rule applies
         if kA > 0 then 
            printfn "  checking rule 3 - A"
            let a = sprintf "(assert (= %s 0))" e2
            if isUnsat enc a then 
               printfn "  was able to prove the 3rd"
               match findMin enc e2 with
               | Some k -> 
                  let m = 
                     if !first then k
                     else min k kA
                  if m > kA' then 
                     learned.[u] <- (kS, m)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
      first := false
      debugLearned learned ti
   None *)

///
///
///
///
/// T0, (S S 1) --> T1, (S S 2)
/// T1, (S S 2) --> T2, (S A 2) <- unq
/// T2, (S A 2) --> T2, (A S 2) <- unq
///

type Label = 
   | S of string
   | A of string
   override this.ToString() = 
      match this with
      | S l -> sprintf "S(%s)" l
      | A l -> sprintf "A(%s)" l

type Inference = 
   | Inference of Label list * int
   override this.ToString() = 
      let (Inference(labels, k)) = this
      let s = Util.List.toString labels
      sprintf "(%s,%d)" s k

let debugLearned2 (learned : Dictionary<_, Inference list>) ti = 
   printfn "========================="
   for kv in learned do
      if CGraph.isRealNode kv.Key then 
         let infs = kv.Value
         let s = Util.List.toString infs
         printfn "%s --> %s" (Topology.router kv.Key.Node.Loc ti) s
   printfn "========================="

let rec mostRecentAncestor vs us = 
   match vs with 
   | [] -> failwith "unreachable"
   | hd::tl -> 
      if List.contains hd us then hd 
      else mostRecentAncestor tl us

let inline getScope (l : Label) = 
   match l with 
   | A x | S x -> x

let rec takeBeyond (labels : Label list) (s : string) = 
   match labels with 
   | [] -> []  
   | hd::tl -> 
      let s' = getScope hd 
      if s = s' then labels
      else takeBeyond tl s

(*
let sameAfter isSome (labels : Label list) (s : string) = 
   let rec aux ls seen =
      match ls with 
      | [] -> [] 
      | hd::tl -> 
         let s' = getScope hd 
         if seen then hd :: aux tl seen 
         else (A s') :: aux tl (s' = s)
   match labels with 
   | [] -> failwith "unreachable"
   | hd::tl ->
      let s' = getScope hd
      let hd' = if isSome then S s' else A s'
      hd' :: (aux tl (s' = s)) *)

let changeFirst isSome ls = 
   match ls with 
   | [] -> failwith "unreachable"
   | hd::tl -> 
      let s = getScope hd
      if isSome then (S s) :: tl else (A s) :: tl

let newLabels (isSome : bool) (labels: Label list) namev nameu (ti : Topology.TopoInfo) =
   //printfn "labels entering: %A" labels
   let vs = ti.EnclosingScopes.[namev]
   let us = ti.EnclosingScopes.[nameu]
   //printfn "enclosing scopes of v: %A" vs
   //printfn "enclosing scopes of u: %A" us
   match Map.tryFind (namev, nameu) ti.NonLocalScopes with 
   | None ->
      let mra = mostRecentAncestor vs us
      // printfn "most recent ancestor: %A" mra
      let labels' = takeBeyond labels mra      
      // printfn "labels': %A" labels'
      let toAdd = List.takeWhile ((<>) mra) us |> List.map A
      // printfn "toAdd: %A" toAdd
      let ret =  changeFirst isSome (toAdd @ labels') // sameAfter isSome labels mra 
      // printfn "ret: %A" ret
      ret
   | Some l -> failwith ""

let rec isStrictlyMoreGeneralThan (xs : Label list) (ys : Label list) = 
   match xs, ys with 
   | [], [] -> true
   | x::xtl, y::ytl -> 
      match x, y with 
      | (S _), (A _) -> false
      | _, _ -> isStrictlyMoreGeneralThan xtl ytl
   | _::_, [] | [], _::_ -> failwith "unreachable"

let addInference (inf : Inference) (learned : Inference list) changed : Inference list * bool = 
   let (Inference(xs,k)) = inf
   let rec aux ls =
      match ls with 
      | [] -> ([inf], true)
      | (Inference(ys,k') as hd) :: tl -> 
         if k' >= k && isStrictlyMoreGeneralThan ys xs then (ls, false)
         else if k >= k' && isStrictlyMoreGeneralThan xs ys then 
            changed := Set.filter (fun (_,inf') -> inf' <> hd) !changed // Major hack
            aux tl
         else 
            let (tl', isAdded) = aux tl
            (hd::tl', isAdded)
   aux learned

let reachability (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : int option = 
   // Capture the base constraints
   let enc = string (baseEncoding ti)
   // Cache results to z3 calls
   let isUnsat = Hashcons.memoize (isUnsat enc)
   let findMin = Hashcons.memoize (findMin enc)
   // Initialize learned clauses
   let learned = Dictionary()
   for n in cg.Graph.Vertices do
      let name = Topology.router n.Node.Loc ti
      match Map.tryFind name ti.EnclosingScopes with
      | None -> ()
      | Some vs -> 
         if n = src then
            let baseInference = List.map S vs
            learned.[n] <- [Inference(baseInference, 1)]
         else learned.[n] <- []
   // Run to a fixed point using inference rules
   printfn "adding initial node: %s" src.Node.Loc
   let baseInf = List.head learned.[src]
   let (Inference(ls,_)) = baseInf
   let changed = ref (Set.singleton (src, baseInf))
   let best = ref (Map.add (src,ls) 1  Map.empty)
   let first = ref true

   let inline pop() = 
      let ret = Set.minElement !changed
      changed := Set.remove ret !changed
      ret
   
   // TODO: retroactively remove changed inferences when a more general one becomes known
   let inline push (x, inf) = 
      printfn "   attempting to add inference: %A" inf
      let vs = learned.[x]
      let (lx, isAdded) = addInference inf vs changed
      if isAdded then
         learned.[x] <- lx
         if isInside x then
            changed := Set.add (x, inf) !changed
   
   let updateBest ls (x,k) = 
      match Map.tryFind (x,ls) !best with 
      | None -> 
         best := Map.add (x,ls) k !best 
         true
      | Some k' -> 
         best := Map.add (x,ls) (max k k') !best
         k > k'

   while (!changed).Count > 0 do
      debugLearned2 learned ti
      let (v, inf) = pop()
      printfn "looking at node: %s" v.Node.Loc
      printfn "looking at inference: %A" inf
      for u in CGraph.neighbors cg v |> Seq.filter CGraph.isRealNode do
         printfn "  looking at neighbor: %s" u.Node.Loc
         let m = getNodeConstraintName ti v
         let n = getNodeConstraintName ti u
         let (e1, e2) = getEdgeConstraintNames ti v u
         // for debugging
         let namev = Topology.router v.Node.Loc ti
         let nameu = Topology.router u.Node.Loc ti
         printfn "  for nodes: (%s,%s)" namev nameu
         printfn "  got labels: (%s,%s,%s,%s)" m n e1 e2
         // Get the existing value k 
         let (Inference(labels,k)) = inf

         // TODO: don't allocate on every loop iteration
         let update k k' isSome = 
            printfn "   found min: %d" k'
            // get the new k value
            let m = 
               if !first then k'
               else min k' k
            printfn "   new min is: %d" m
            // update the best
            let labels' = newLabels isSome labels namev nameu ti 
            printfn "   new labels: %A" labels'
            if updateBest labels' (u,m) then
               printfn "   better than before"
               let newFact = Inference(labels',m)
               printfn "   new derived fact: %A" newFact
               push (u,newFact)

         match List.head labels with 
         | (S _) as l ->
            // Rule 2
            let a = sprintf "(assert (not (= %s %s)))" e1 n
            if isUnsat a then 
               printfn "   Rule 2(S)"
               match findMin n with 
               | Some k' -> update k k' false
               | None -> ()
            // Rule 3
            printfn "  checking rule 1 - S"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat a then 
               printfn "   Rule 1(S)"
               match findMin e1 with
               | Some k' -> update k k' true
               | None -> () // TODO: error case
         | A _ -> 
            // Rule 1
            let a = sprintf "(assert (= %s 0))" e2
            if isUnsat a then 
               printfn "   Rule 3(A)"
               match findMin e2, findMin n with
               | Some k1, Some k2 -> update k (k1*k2) false
               | _, _ -> ()
            // Rule 2
            let a = sprintf "(assert (not (= %s %s)))" e1 n
            if isUnsat a then 
               printfn "   Rule 2(A)"
               match findMin n with 
               | Some k' -> update k k' false
               | None -> ()
            // Rule 3
            printfn "  checking rule 1 - S"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat a then 
               printfn "   Rule 1(A)"
               match findMin e1 with
               | Some k' -> update k k' true
               | None -> () // TODO: error case
      first := false
      debugLearned2 learned ti
   None

(* 

TODO LIST
- Ensure the number of pods is at least 1, ... does this actually matter?
- Implementation for non-local scopes
- Well-formedness for concrete pods

*)