module AbstractAnalysis

open CGraph
open Core.Printf
open System.Collections.Generic
open System.Text
open Util.Format
open Util.Debug

(* 

TODO LIST
- Ensure the number of pods is at least 1, ... does this actually matter?
- Well-formedness for concrete pods

*)

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
      let (a1,a2) = kv.Key
      for e in kv.Value do
         if e.Source = a1 then

           writeDeclaration sb e.Label
   for n in ti.PodLabels do 
      writeDeclaration sb n
   for kv in ti.NodeLabels do
      let n = kv.Value
      bprintf sb "(assert (>= %s 0))\n" n
   for kv in ti.EdgeLabels do
      let (a1,a2) = kv.Key
      for e in kv.Value do 
         if e.Source = a1 then
           bprintf sb "(assert (>= %s 0))\n" e.Label
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
         if e.Source = a1 then
           bprintf sb "(assert (>= %s %s))\n" labelN e.Label
   sb

let inline getNodeConstraintName (ti : Topology.TopoInfo) v = 
   let name = Topology.router v.Node.Loc ti
   ti.NodeLabels.[name]

let inline getEdgeConstraintNames (ti : Topology.TopoInfo) v u = 
   let namev = Topology.router v.Node.Loc ti
   let nameu = Topology.router u.Node.Loc ti
   ti.EdgeLabels.[(namev, nameu)]

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
      let (a1,a2) = kv.Key
      let label = ti.EdgeLabels.[(a1,a2)]
      for e in label do
         if e.Source = a1 then 
           bprintf sb "(assert (>= %s %d))\n" e.Label kv.Value
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
   | Inference of Label list * int * bool
   override this.ToString() = 
      let (Inference(labels, k, group)) = this
      let s = Util.List.toString labels
      sprintf "(%s,%d,%s)" s k (if group then "group" else "single")

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

let rec takeBeyond (labels : Label list) (s : string) = 
   match labels with 
   | [] -> []  
   | hd::tl -> 
      let s' = getScope hd 
      if s = s' then labels
      else takeBeyond tl s

let changeFirst isSome ls = 
   match ls with 
   | [] -> failwith "unreachable"
   | hd::tl -> 
      let s = getScope hd
      if isSome then (S s) :: tl else (A s) :: tl

let rec refill (xs : string list) (ys : Topology.CustomLabel list) = 
   match xs, ys with 
   | [], [] -> []
   | x::xtl, [] -> A x :: (refill xtl ys)
   | [], _::_ -> failwith "unreachable"
   | x::xtl, y::ytl -> 
      match y with 
      | Topology.SomeLabel _ -> S x :: (refill xtl ytl)
      | Topology.AllLabel _ -> A x :: (refill xtl ytl)
      | _ -> failwith "unreachable"

let newLabels (isSome : bool) (labels: Label list) (e : Topology.EdgeInfo) (namev, nameu) (ti : Topology.TopoInfo) =
   log (sprintf "     labels entering: %A" labels)
   let vs = ti.EnclosingScopes.[namev]
   let us = ti.EnclosingScopes.[nameu]
   log (sprintf "     enclosing scopes of v: %A" vs)
   log (sprintf "     enclosing scopes of u: %A" us)
   let ancestor = e.Scope
   log (sprintf "     scope: %A" ancestor)
   let nonLocal = e.Scope <> (List.head vs) || e.Scope <> (List.head us)
   log (sprintf "     nonLocal: %A" nonLocal)
   let labels' = takeBeyond labels ancestor
   let labels' = if nonLocal then changeFirst false labels' else labels' 
   log (sprintf "     labels': %A" labels')
   let toRefill = List.takeWhile ((<>) ancestor) us
   let toAdd = refill toRefill (List.tail e.Back)
   log (sprintf "     toAdd: %A" toAdd)
   let ret =  changeFirst isSome ((S nameu)::(toAdd @ labels'))
   log (sprintf "     ret: %A" ret)
   ret

let rec isStrictlyMoreGeneralThan (xs : Label list) (ys : Label list) = 
   match xs, ys with 
   | [], [] -> true
   | x::xtl, y::ytl -> 
      match x, y with 
      | (S _), (A _) -> false
      | _, _ -> isStrictlyMoreGeneralThan xtl ytl
   | _::_, [] | [], _::_ -> failwith "unreachable"

let addInference (inf : Inference) (learned : Inference list) changed : Inference list * bool = 
   let (Inference(xs,k,bx)) = inf
   let rec aux ls =
      match ls with 
      | [] -> ([inf], true)
      | (Inference(ys,k',by) as hd) :: tl -> 
         if by = bx && k' >= k && isStrictlyMoreGeneralThan ys xs then (ls, false)
         else if by = bx && k >= k' && isStrictlyMoreGeneralThan xs ys then 
            changed := Set.filter (fun (_,inf',_) -> inf' <> hd) !changed // Major hack
            aux tl
         else 
            let (tl', isAdded) = aux tl
            (hd::tl', isAdded)
   aux learned

let inferenceRelevant (inf : Inference) (e : Topology.EdgeInfo) =
   let (Inference(labels,k,_)) = inf  
   // printfn "inf: %A" labels
   // printfn "e: %A" e
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
   // printfn "Required: %A" required
   // printfn "Given: %A" given
   let canApply = Set.isEmpty (Set.difference required given)
   let isExistential = 
      match List.head e.Front with 
      | Topology.AllLabel _ -> false 
      | Topology.SomeLabel _ -> true
      | _ -> failwith "unreachable"
   // printfn "can apply: %A" canApply
   // printfn "is existential: %A" isExistential
   (canApply, isExistential)

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
            let name = Topology.router n.Node.Loc ti
            let baseInference = List.map S (name::vs)
            learned.[n] <- [Inference(baseInference, 0, false)]
         else learned.[n] <- []
   // Run to a fixed point using inference rules
   log (sprintf "adding initial node: %s" src.Node.Loc)
   let baseInf = List.head learned.[src]
   let (Inference(ls,_,_)) = baseInf
   let changed = ref (Set.singleton (src, baseInf, Set.empty))
   let best = ref (Map.add (src,ls,false) (0,Set.empty) Map.empty)
   let first = ref true

   let inline pop() = 
      let ret = Set.minElement !changed
      changed := Set.remove ret !changed
      ret
   
   let inline push (v,u,edges) inf = 
      log (sprintf "   attempting to add inference: %A" inf)
      let vs = learned.[u]
      let (lx, isAdded) = addInference inf vs changed
      if isAdded then
         learned.[u] <- lx
         if isInside u then
            let es = Set.add (v,u) edges |> Set.add (u,v)
            changed := Set.add (u, inf, es) !changed
   
   let updateBest ls isGroup (v,u) k edges = 
      let es = Set.add (v,u) edges |> Set.add (u,v)
      match Map.tryFind (u,ls,isGroup) !best with 
      | None -> 
         best := Map.add (u,ls,isGroup) (k,es) !best 
         true, k
      | Some (k',edges') ->
         if Set.intersect es edges' |> Set.isEmpty then
            best := Map.add (u,ls,isGroup) (k+k', Set.union es edges') !best
            true, k+k'
         else if k' >= k then false, k
         else 
            best := Map.add (u,ls,isGroup) (k,es) !best
            true, k

   while (!changed).Count > 0 do
      let (v, inf, edges) = pop()
      log (sprintf "looking at inference: %A" inf)
      log (sprintf "edges so far: %A" (Set.map (fun (x,y) -> (string x, string y)) edges))
      // Get the existing value k 
      let (Inference(labels,k,isGroup)) = inf
      for u in CGraph.neighbors cg v |> Seq.filter CGraph.isRealNode do
         debugLearned learned ti
         let m = getNodeConstraintName ti v

         let n = getNodeConstraintName ti u
         let es = getEdgeConstraintNames ti v u
         // for debugging
         let namev = Topology.router v.Node.Loc ti
         let nameu = Topology.router u.Node.Loc ti

         for e in es do            
            log (sprintf "  for nodes: (%s,%s)" namev nameu)
            log (sprintf "  got labels: (%s,%s,%s)" m n e.Label)
            let isRelevant, isExistential = inferenceRelevant inf e
            if isRelevant then
               log "  is relevant"
               log (sprintf "  is existential? %A" isExistential)
               
               // TODO: don't allocate on every loop iteration
               let update k k' isSome isGroup = 
                  log (sprintf "   found min: %d" k')
                  // get the new k value
                  let m = 
                     if !first then k'
                     else min k' k
                  log (sprintf "   new min is: %d" m)
                  // update the best
                  let labels' = newLabels isSome labels e (namev,nameu) ti 
                  log (sprintf "   new labels: %A" labels')
                  let isBetter, value = updateBest labels' isGroup (v,u) m edges
                  if isBetter then
                     log (sprintf "   better than before: %d" value)
                     let newFact = Inference(labels',value,isGroup)
                     log (sprintf "   new derived fact: %A" newFact)
                     push (v,u,edges) newFact

               let isSource = (e.Source = namev)
               let isTarget = (e.Target = namev)
   
               match List.head labels, isExistential with 
               | S _, false ->
                  if isSource then 
                     let e1 = e.Label
                     // Rule 2
                     let a = sprintf "(assert (not (= %s %s)))" e1 n
                     if isUnsat a then 
                        log "   Rule 2(S)"
                        match findMin n with 
                        | Some k' ->
                           update k 1 false false
                           update k k' false true
                        | None -> ()
                     // Rule 3
                     let a = sprintf "(assert (= %s 0))" e1
                     if isUnsat a then 
                        log "   Rule 1(S)"
                        match findMin e1 with
                        | Some k' ->
                           update k 1 true false
                           update k k' true true
                        | None -> ()
               | A _, _ -> 
                  if isTarget then
                     let e2 = e.Label
                     // Rule 1
                     let a = sprintf "(assert (= %s 0))" e2
                     if isUnsat a then 
                        log "   Rule 1(A)"
                        match findMin e2, findMin n with
                        | Some k1, Some k2 -> 
                           let k' = if isGroup then k1 else k1*k2
                           update k k1 false false
                           update k k' false true
                        | _, _ -> ()
                  if isSource then
                     let e1 = e.Label
                     // Rule 2
                     let a = sprintf "(assert (not (= %s %s)))" e1 n
                     if isUnsat a then 
                        log "   Rule 2(A)"
                        match findMin n with 
                        | Some k' ->
                           let k' = (if isExistential && isGroup then 1 else k')
                           update k 1 false false
                           update k k' false true
                        | None -> ()
                     // Rule 3
                     let a = sprintf "(assert (= %s 0))" e1
                     if isUnsat a then 
                        log "   Rule 3(A)"
                        match findMin e1 with
                        | Some k' ->
                           let k' = (if isExistential && isGroup then 1 else k')
                           update k 1 true false
                           update k k' true true 
                        | None -> ()
               | _, _ -> ()
      first := false
      debugLearned learned ti
   None