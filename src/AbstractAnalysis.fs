module AbstractAnalysis

open CGraph
open Core.Printf
open System.Collections.Generic
open System.Text

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

let inline writeWellFormedness sb name = bprintf sb "(assert (>= %s 0))\n" name

let inline writeConstraints sb (c : Topology.Constraint) = 
   writeWellFormedness sb c.Name
   writeFormula sb c.Formula

let baseEncoding (ti : Topology.TopoInfo) = 
   let sb = StringBuilder()
   for kv in ti.NodeConstraints do
      let c = kv.Value
      writeDeclaration sb c.Name
   for kv in ti.EdgeConstraints do
      let (c1, c2) = kv.Value
      writeDeclaration sb c1.Name
   for kv in ti.NodeConstraints do
      let c = kv.Value
      writeConstraints sb c
   for kv in ti.EdgeConstraints do
      let (c1, c2) = kv.Value
      writeConstraints sb c1
   sb

let inline getNodeConstraintName (ti : Topology.TopoInfo) v = 
   let name = Topology.router v.Node.Loc ti
   ti.NodeConstraints.[name].Name

let inline getEdgeConstraintNames (ti : Topology.TopoInfo) v u = 
   let namev = Topology.router v.Node.Loc ti
   let nameu = Topology.router u.Node.Loc ti
   let (e1, e2) = ti.EdgeConstraints.[(namev, nameu)]
   (e1.Name, e2.Name)

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
   | _ -> false

let findMin enc x = 
   let check = enc + (minimumEncoding x)
   match Z3.run check with
   | Z3.Minimized k -> Some k
   | _ -> None

let debugLearned (learned : Dictionary<_, _>) ti = 
   let pretty x = 
      if x = -1 then "UNREACH"
      else string x
   printfn "========================="
   for kv in learned do
      let (s, a) = kv.Value
      printfn "%s --> (%s,%s)" (Topology.router kv.Key.Node.Loc ti) (pretty s) (pretty a)
   printfn "========================="

let reachability (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : int option = 
   printfn "Got call for src: %s" (string src)
   // Capture the base constraints
   let enc = string (baseEncoding ti)
   // Start with infinity for S and A labels
   let learned = Dictionary()
   for n in cg.Graph.Vertices do
      if n = src then learned.[n] <- (1, -1)
      else learned.[n] <- (-1, -1)
   // Run to a fixed point using inference rules
   printfn "adding initial node: %s" src.Node.Loc
   let changed = ref (Set.singleton src)
   
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
         //
         printfn "  got labels: (%s,%s,%s,%s)" m n e1 e2
         // if we have an A label, then we have an S label
         if kS < kA then learned.[v] <- (kA, kA)
         // lookup existing values of k
         // if third rule applies
         if kS > 0 then 
            printfn "  checking rule 1 - S"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat enc a then 
               printfn "   Rule 1-1"
               match findMin enc e1 with
               | Some k -> 
                  printfn "   found min: %d" k
                  if k > kS' then 
                     learned.[u] <- (k, kA)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
         if kA > 0 then 
            printfn "  checking rule 1 - A"
            let a = sprintf "(assert (= %s 0))" e1
            if isUnsat enc a then 
               printfn "   Rule 1-2"
               match findMin enc e1 with
               | Some k -> 
                  if k > kS' then 
                     learned.[u] <- (k, kA)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
         // if second rule applies
         if kS > 0 then 
            printfn "  checking rule 2 - S"
            let a = sprintf "(assert (not (= %s %s)))" e1 n
            if isUnsat enc a then 
               printfn "   Rule 2-1"
               match findMin enc n with
               | Some k -> 
                  printfn "   found min: %d" k
                  if k > kA' then 
                     learned.[u] <- (kS, k)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
         if kA > 0 then 
            printfn "  checking rule 1 - A"
            let a = sprintf "(assert (= %s %s))" e1 n
            if isUnsat enc a then 
               printfn "   Rule 2-2"
               match findMin enc n with
               | Some k -> 
                  if k > kA' then 
                     learned.[u] <- (kS, k)
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
                  if k > kA' then 
                     learned.[u] <- (kS, k)
                     printfn "     adding: %s to the stack" (Topology.router u.Node.Loc ti)
                     push u
               | None -> ()
      debugLearned learned ti
   None
