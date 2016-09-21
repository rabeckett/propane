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

let INF = System.Int32.MaxValue

let inline getNodeConstraintName (ti : Topology.TopoInfo) v = 
   let name = Topology.router v.Node.Loc ti
   printfn "%s" name
   ti.NodeConstraints.[name].Name

let inline getEdgeConstraintNames (ti : Topology.TopoInfo) v u = 
   let namev = Topology.router v.Node.Loc ti
   let nameu = Topology.router u.Node.Loc ti
   printfn "(%s,%s)" namev nameu
   let (e1, e2) = ti.EdgeConstraints.[(namev, nameu)]
   (e1.Name, e2.Name)

let minimumEncoding e = 
   let x = sprintf "(declare-const m Int)\n"
   let y = sprintf "(assert (= m %s))\n" e
   let z = sprintf "(minimize m)\n"
   x + y + z

let reachability (ti : Topology.TopoInfo) (cg : CGraph.T) (src : CgState) : int option = 
   printfn "Got call for src: %s" (string src)
   // Capture the base constraints
   let enc = string (baseEncoding ti)
   // Start with infinity for S and A labels
   let learned = Dictionary()
   for n in cg.Graph.Vertices do
      learned.[n] <- (INF, INF)
   // Run to a fixed point using inference rules
   printfn "adding initial node: %s" src.Node.Loc
   let stack = Stack()
   stack.Push(src)
   while stack.Count > 0 do
      let v = stack.Pop()
      printfn "looking at node: %s" v.Node.Loc
      let (kS, kA) = learned.[v]
      for u in CGraph.neighbors cg v |> Seq.filter CGraph.isInside do
         printfn "  looking at neighbor: %s" u.Node.Loc
         let m = getNodeConstraintName ti v
         let n = getNodeConstraintName ti u
         let (e1, e2) = getEdgeConstraintNames ti v u
         printfn "  got labels: (%s,%s,%s,%s)" m n e1 e2
         // lookup existing values of k
         // if first rule applies
         if kS < INF then 
            let constr = sprintf "(assert (= %s 0))\n" e1
            let check = enc + constr
            match Z3.run check with
            | Z3.Unsat -> 
               printfn "was able to prove 1st"
               let check = enc + (minimumEncoding e1)
               match Z3.run check with
               | Z3.Minimized k -> 
                  if k < kS then 
                     learned.[v] <- (k, kA)
                     stack.Push(u)
               | _ -> ()
            | _ -> ()
         // if third rule applies
         if kA < INF then 
            let constr = sprintf "(assert (= %s 0))\n" e2
            let check = enc + constr
            match Z3.run check with
            | Z3.Unsat -> 
               printfn "  was able to prove the 3rd"
               let check = enc + (minimumEncoding e2)
               match Z3.run check with
               | Z3.Minimized k -> 
                  if k < kA then 
                     learned.[v] <- (kS, k)
                     stack.Push(u)
                     printfn "Found better new minimum: %d" k
               | _ -> ()
            | _ -> ()
   None
