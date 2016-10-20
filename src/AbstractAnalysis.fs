module AbstractAnalysis

open CGraph
open Core.Printf
open System.Collections.Generic
open System.Text
open Util.Debug
open Util.Format

module Z3 = 
   type Result = 
      | Error of string
      | Minimized of int
      | Sat
      | Unsat
   
   let run (input : string) : Result = 
      let cmds = [ "z3"; "-smt2"; "-in"; "-t:200" ]
      let input = input + "(check-sat)"
      let output = Util.Command.run cmds (Some input)
      match output with
      | None -> failwith "invalid command"
      | Some txt -> 
         if txt.StartsWith("sat") then 
            let lines = Util.String.toLines txt
            if lines.Length >= 3 then 
               let l = lines.[2]
               match Util.String.sscanf " (minval %d)" l with
               | Some x -> Minimized x
               | _ -> Sat
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
      let (a1, a2) = kv.Key
      for e in kv.Value do
         if e.Source = a1 then writeDeclaration sb e.Label
   for n in ti.PodLabels do
      writeDeclaration sb n
   for kv in ti.NodeLabels do
      let n = kv.Value
      bprintf sb "(assert (>= %s 0))\n" n
   for kv in ti.EdgeLabels do
      let (a1, a2) = kv.Key
      for e in kv.Value do
         if e.Source = a1 then bprintf sb "(assert (>= %s 0))\n" e.Label
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
      let labels = ti.EdgeLabels.[(a1, a2)]
      for e in labels do
         if e.Source = a1 then bprintf sb "(assert (>= %s %s))\n" labelN e.Label
   sb

let inline getNodeConstraintName (ti : Topology.TopoInfo) v = 
   let name = Topology.router v.Node.Loc ti
   ti.NodeLabels.[name]

let inline getEdgeConstraintNames (ti : Topology.TopoInfo) v u = 
   let namev = Topology.router v.Node.Loc ti
   let nameu = Topology.router u.Node.Loc ti
   ti.EdgeLabels.[(namev, nameu)]

let minimumEncoding e = 
   let x = sprintf "(declare-const minval Int)\n"
   let y = sprintf "(assert (= minval %s))\n" e
   let z = sprintf "(minimize minval)\n"
   x + y + z

let isUnsat enc a = 
   let constr = sprintf "%s\n" a
   let check = enc + constr
   match Z3.run check with
   | Z3.Unsat -> true
   | Z3.Sat -> false
   | Z3.Error s -> error (sprintf "Invalid command: %s" s)
   | Z3.Minimized k -> error (sprintf "Invalid result: minimized")

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
      let (a1, a2) = kv.Key
      let label = ti.EdgeLabels.[(a1, a2)]
      for e in label do
         if e.Source = a1 then bprintf sb "(assert (>= %s %d))\n" e.Label kv.Value
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

/// Abstract k-disjoint path analysis
/// 
/// Uses abstract interpretation to find a minimum bound on 
/// the number of disjoint paths from a src to all other destinations.
/// The analysis will return the minimum for all possible concrete
/// topologies satisfying the topology constraints.
type Label = 
   | S of string
   | A of string
   override this.ToString() = 
      match this with
      | S l -> sprintf "S(%s)" l
      | A l -> sprintf "A(%s)" l

type Inference = 
   | Inference of Label list * int * int * Set<CgState * CgState * Label list>
   override this.ToString() = 
      let (Inference(labels, j, k, _)) = this
      let s = Util.List.toString labels
      sprintf "%s (%d,%d)" s j k

let debugLearned (learned : Dictionary<_, Inference list>) ti = 
   log "========================="
   for kv in learned do
      if CGraph.isRealNode kv.Key then 
         let infs = kv.Value
         let s = Util.List.toString infs
         log (sprintf "%s --> %s" (Topology.router kv.Key.Node.Loc ti) s)
   log "========================="

let inline getScope (l : Label) = 
   match l with
   | A x | S x -> x

let inline isAll (l : Label) = 
   match l with
   | A _ -> true
   | S _ -> false

let rec takeBeyond (labels : Label list) (s : string) = 
   match labels with
   | [] -> []
   | hd :: tl -> 
      let s' = getScope hd
      if s = s' then labels
      else takeBeyond tl s

let changeFirst isSome ls = 
   match ls with
   | [] -> failwith "unreachable"
   | hd :: tl -> 
      let s = getScope hd
      if isSome then (S s) :: tl
      else (A s) :: tl

let rec refill (xs : string list) (ys : Topology.CustomLabel list) = 
   match xs, ys with
   | [], [] -> []
   | x :: xtl, [] -> A x :: (refill xtl ys)
   | [], _ :: _ -> failwith "unreachable"
   | x :: xtl, y :: ytl -> 
      match y with
      | Topology.SomeLabel _ -> S x :: (refill xtl ytl)
      | Topology.AllLabel _ -> A x :: (refill xtl ytl)
      | _ -> failwith "unreachable"

let newLabels (isSome : bool) (labels : Label list) (e : Topology.EdgeInfo) (namev, nameu) 
    (ti : Topology.TopoInfo) = 
   //log (sprintf "     labels entering: %A" labels)
   let vs = ti.EnclosingScopes.[namev]
   let us = ti.EnclosingScopes.[nameu]
   //log (sprintf "     enclosing scopes of v: %A" vs)
   //log (sprintf "     enclosing scopes of u: %A" us)
   let ancestor = e.Scope
   //log (sprintf "     scope: %A" ancestor)
   let nonLocal = e.Scope <> (List.head vs) || e.Scope <> (List.head us)
   //log (sprintf "     nonLocal: %A" nonLocal)
   let labels' = takeBeyond labels ancestor
   
   let labels' = 
      if nonLocal then changeFirst false labels'
      else labels'
   
   //log (sprintf "     labels': %A" labels')
   let toRefill = List.takeWhile ((<>) ancestor) us
   let toAdd = refill toRefill (List.tail e.Back)
   //log (sprintf "     toAdd: %A" toAdd)
   let ret = changeFirst isSome ((S nameu) :: (toAdd @ labels'))
   //log (sprintf "     ret: %A" ret)
   ret

let rec betterLabels (xs : Label list) (ys : Label list) = 
   match xs, ys with
   | [], [] -> true
   | x :: xtl, y :: ytl -> 
      match x, y with
      | (S _), (A _) -> false
      | _, _ -> xtl = ytl // betterLabels xtl ytl
   | _ :: _, [] | [], _ :: _ -> failwith "unreachable"

let inline isStrictlyBetter (j, k, xs) (j', k', ys) = (j = j' && k >= k' && betterLabels xs ys) //(List.tail (List.rev xs) = List.tail (List.rev ys)))

let addInference (inf : Inference) (learned : Inference list) changed : Inference list * bool = 
   let (Inference(xs, j, k, es)) = inf
   
   let rec aux ls = 
      match ls with
      | [] -> ([ inf ], true)
      | (Inference(ys, j', k', es') as hd) :: tl -> 
         if isStrictlyBetter (j', k', ys) (j, k, xs) then (ls, false)
         else if isStrictlyBetter (j, k, xs) (j', k', ys) then 
            // remove pending learned facts if strictly worse
            changed := Set.filter (fun (_, inf') -> inf' <> hd) !changed
            aux tl
         else 
            let (tl', isAdded) = aux tl
            (hd :: tl', isAdded)
   if j <= 0 || k <= 0 then (learned, false)
   else aux learned

let inferenceRelevant (inf : Inference) (e : Topology.EdgeInfo) = 
   let (Inference(labels, _, _, _)) = inf
   let mutable required = Set.empty
   for v in e.Front do
      match v with
      | Topology.SomeLabel x -> required <- Set.add x required
      | _ -> ()
   let mutable given = Set.empty
   for l in labels do
      match l with
      | A x -> given <- Set.add x given
      | S _ -> ()
   let canApply = Set.isEmpty (Set.difference required given)
   
   let isExistential = 
      match List.head e.Front with
      | Topology.AllLabel _ -> false
      | Topology.SomeLabel _ -> true
      | _ -> failwith "unreachable"
   (canApply, isExistential)

let inline nodeUsed (x : CgState) (edges : Set<CgState * CgState>) = false //Seq.exists (fun (a,b) -> a = x || b = x) edges

let inline removeGlobal inf = 
   let (Inference(ls, j, k, es)) = inf
   
   let ls' = 
      List.rev ls
      |> List.tail
      |> List.rev
   Inference(ls', j, k, es)

let inline greatest xs = 
   let mutable acc = 0
   for (Inference(_, _, k, _)) in xs do
      acc <- max acc k
   acc

let combineAllInferences (learned : Dictionary<CgState, Inference list>) = 
   let mutable acc = Map.empty
   let numLabels = ref 0
   for kv in learned do
      let v = kv.Key
      let allInfs = kv.Value |> List.map removeGlobal
      
      let all = 
         allInfs
         |> List.filter (fun (Inference(ls, _, _, _)) -> 
               numLabels := List.length ls - 1
               isAll ls.Head)
         |> Seq.groupBy (fun (Inference(ls, _, _, _)) -> ls)
         |> Seq.map (fun (_, ys) -> greatest ys)
      
      let mutable minAll = System.Int32.MaxValue
      let mutable maxSome = 0
      for k in all do
         minAll <- min minAll k
      for inf in allInfs do
         let (Inference(ls, j, k, es)) = inf
         maxSome <- max maxSome k
      let x = 
         if minAll = System.Int32.MaxValue || Seq.length all < pown 2 !numLabels then 0
         else minAll
      acc <- Map.add v (x, maxSome) acc
   acc

type AnalysisResult = Map<CgState, int * int>

let reachability (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : AnalysisResult = 
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
            let name = Topology.router n.Node.Loc ti
            let baseInference = List.map S (name :: vs)
            learned.[n] <- [ Inference(baseInference, 1, 1, Set.empty) ]
         else learned.[n] <- []
   // Run to a fixed point using inference rules
   let baseInf = List.head learned.[src]
   let (Inference(ls, _, _, _)) = baseInf
   let best = ref Map.empty
   let changed = ref (Set.singleton (src, baseInf))
   let first = ref true
   
   let pop() = 
      let ret = Set.minElement !changed
      changed := Set.remove ret !changed
      ret
   
   let push (_, u) inf = 
      let (Inference(_, _, _, edges)) = inf
      log (sprintf "   attempting to add inference: %s" (string inf))
      let vs = learned.[u]
      let (lx, isAdded) = addInference inf vs changed
      if isAdded then 
         log (sprintf "   adding inference")
         learned.[u] <- lx
         if isInside u then changed := Set.add (u, inf) !changed
   
   let updateBest ls (j, k) (v, u) edges = 
      let ls' = 
         List.tail ls
         |> List.rev
         |> List.tail
      
      let es = Set.add (v, u, ls') edges // |> Set.add (u,v,ls')
      match Map.tryFind (u, ls, j) !best with
      | None -> 
         best := Map.add (u, ls, j) (k, es) !best
         true, (j, k, es)
      | Some(k', edges') -> 
         if Set.isEmpty (Set.intersect es edges') && isAll (List.head ls) then 
            let both = Set.union es edges'
            best := Map.add (u, ls, j) (k + k', both) !best
            true, (j, k + k', both)
         else if k' >= k then false, (j, k', edges')
         else 
            best := Map.add (u, ls, j) (k, es) !best
            true, (j, k, es)
   
   let updateAux useNewLabels (labels, e, edges) (namev, nameu) (v, u) (j, k) isSome = 
      log (sprintf "   adding fact for: (%d,%d)" j k)
      // update the best
      let labels' = 
         if useNewLabels then newLabels isSome labels e (namev, nameu) ti
         else labels
      log (sprintf "   new labels: %A" labels')
      let isBetter, (j', k', edges') = updateBest labels' (j, k) (v, u) edges
      if isBetter then 
         log (sprintf "   better than before: (%d,%d)" j k)
         let newFact = Inference(labels', j', k', edges')
         log (sprintf "   new derived fact: %s" (string newFact))
         push (v, u) newFact
   
   while (!changed).Count > 0 do
      let (v, inf) = pop()
      // Get the existing value k 
      let (Inference(labels, j, k, edges)) = inf
      log (sprintf "looking at inference: %s" (string inf))
      for u in CGraph.neighbors cg v |> Seq.filter CGraph.isRealNode do
         debugLearned learned ti
         let m = getNodeConstraintName ti v
         let n = getNodeConstraintName ti u
         let es = getEdgeConstraintNames ti v u
         // for debugging
         let namev = Topology.router v.Node.Loc ti
         let nameu = Topology.router u.Node.Loc ti
         for e in es do
            log (sprintf " for nodes: (%s,%s)" namev nameu)
            log (sprintf " got labels: (%s,%s,%s)" m n e.Label)
            let isRelevant, isExistential = inferenceRelevant inf e
            if isRelevant then 
               log "  is relevant"
               log (sprintf "  is existential? %A" isExistential)
               let update = updateAux true (labels, e, edges)
               let isSource = (e.Source = namev)
               let isTarget = (e.Target = namev)
               
               let weakMin x y = 
                  if !first then y
                  else min x y
               // Special case for src
               if v = src && isAll (List.head labels) then 
                  let a = sprintf "(assert (<= %s 1))" m
                  if isUnsat a then 
                     log "  Rule SPECIAL CASE"
                     let labels' = List.map (getScope >> S) labels
                     updateAux false (labels', e, edges) (namev, namev) (v, v) (2, 1) true
               // Existential Case
               if isExistential && isSource then 
                  match List.head labels with
                  | A _ -> 
                     let e1 = e.Label
                     let a1 = sprintf "(assert (= %s 0))" e1
                     let a2 = sprintf "(assert (not (= %s %s)))" e1 n
                     let b1, b2 = isUnsat a1, isUnsat a2
                     if b1 || b2 then 
                        log "  Rule existential"
                        match findMin e1 with
                        | Some z -> update (namev, nameu) (v, u) (weakMin k z, 1) (not b2)
                        | None -> ()
                  | _ -> ()
               else 
                  // Striping patterns
                  match isSource, e.Label, e.OtherLabel with
                  | true, e1, Some e2 | false, e2, Some e1 -> 
                     let a1 = sprintf "(assert (= %s 0))" e1
                     let a2 = sprintf "(assert (= %s 0))" e2
                     if isUnsat a1 && isUnsat a2 then 
                        log "  Rule striping 1"
                        let s = sprintf "(- %s (div (* (- %s %d) %s) %s))" n m j e1 e2
                        match findMin s with
                        | Some z -> update (namev, nameu) (v, u) (weakMin (j * k) z, 1) true
                        | _ -> ()
                        log "  Rule striping 2"
                        let s = sprintf "(- %d (mod %s (div %s %s)))" j m n e1
                        match findMin e1, findMin e2, findMin s with
                        | Some ze1, Some ze2, Some zo -> 
                           update (namev, namev) (v, v) (zo, min k (min ze1 ze2)) 
                              (not (isAll (List.head labels)))
                        | _ -> ()
                  | _, _, _ -> ()
                  // Other Inference rules
                  if isSource then 
                     let e1 = e.Label
                     // Rule 1
                     let a = sprintf "(assert (= %s 0))" e1
                     if isUnsat a then 
                        log "  Rule e1>0"
                        match findMin e1 with
                        | Some z -> update (namev, nameu) (v, u) (weakMin (j * k) z, 1) true
                        | _ -> ()
                        // Rule 2
                        let a = sprintf "(assert (not (= %s %s)))" e1 n
                        if isUnsat a then 
                           log "  Rule e1=n"
                           match findMin e1 with
                           | Some z -> 
                              let t = weakMin (j * k) z
                              update (namev, nameu) (v, u) (t, max 1 ((j * k) / t)) false
                              update (namev, nameu) (v, u) (1, min j z) false
                           | _ -> ()
                  if isTarget then 
                     let e2 = e.Label
                     // Rule 1
                     let a = sprintf "(assert (= %s 0))" e2
                     if isAll (List.head labels) && isUnsat a then 
                        log "  Rule e2>0"
                        match findMin e2 with
                        | Some z -> update (namev, nameu) (v, u) (1, min j z) false
                        | _ -> ()
                        // Rule 2
                        let a1 = sprintf "(assert (not (= %s %s)))" e2 m
                        let a2 = sprintf "(assert (= %s 0))" n
                        if isUnsat a1 && isUnsat a2 then 
                           log "  Rule e2=m"
                           match findMin e2, findMin n with
                           | Some ze, Some zn -> 
                              let t = weakMin (j * k) zn
                              update (namev, nameu) (v, u) (t, max 1 ((j * k) / t)) false
                              update (namev, nameu) (v, u) (1, min j ze) false
                           | _ -> ()
      first := false
      debugLearned learned ti
   let ret = combineAllInferences learned
   for kv in ret do
      let v = Topology.router kv.Key.Node.Loc ti
      let (x, y) = kv.Value
      log (sprintf "%s --> (all=%d, some=%d)" v x y)
   ret