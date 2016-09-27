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
   for kv in ti.NodeLabels do
      let n = kv.Value
      bprintf sb "(assert (>= %s 0))\n" n
   for kv in ti.EdgeLabels do
      let n = kv.Value
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

let reachability (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : int option = 
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
         (* if kA > 0 then 
            printfn "  checking rule 1 - A"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat enc a then 
               printfn "   Rule 1-2"
               match findMin enc e1 with
               | Some k -> 
                  let m = 
                     if !first then k
                     else min k kA
                  if m > kS' then 
                     learned.[u] <- (m, kA)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> () *)
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
         (*if kA > 0 then 
            printfn "  checking rule 1 - A"
            let a = sprintf "(assert (= %s %s))" e1 n
            if isUnsat enc a then 
               printfn "   Rule 2-2"
               match findMin enc n with
               | Some k -> 
                  let m = 
                     if !first then k
                     else min k kA
                  if m > kA' then 
                     learned.[u] <- (kS, m)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> () *)
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
   None
