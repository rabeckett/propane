module Hashcons

open System
open System.Collections.Generic

let private CACHE_SIZE = 2048

/// A hashcons node is a separate type to ensure safety
/// since there is only one way to create such a node.
/// It contains a unique id for comparison, a cached hash
/// to make hashing an O(1) operation for new nodes, and 
/// the pointer to the value so that traversal does not require
/// hash table lookups
[<CustomComparison; CustomEquality>]
type HashCons<'a when 'a : equality> = 
   struct
      val Id : int
      val Hash : int
      val Node : 'a
      
      new(id, node) = 
         { Id = id
           Hash = hash node
           Node = node }
      
      override x.ToString() = x.Node.ToString()
      
      override x.Equals(other) = 
         match other with
         | :? HashCons<'a> as y -> (x.Id = y.Id)
         | _ -> false
      
      override x.GetHashCode() = x.Hash
      interface System.IComparable with
         member x.CompareTo other = 
            match other with
            | :? HashCons<'a> as y -> x.Id - y.Id
            | _ -> failwith "cannot compare values of different types"
   end

/// An object that contains the unique hashcons table
/// and ensures building a hashcons node checks for unicity.
/// Creation uses locking to avoid cache contention with parallelism.
type HashConsBuilder<'a when 'a : equality> = 
   class
      val mutable CurrIndex : int
      val UnqTable : Dictionary<'a, HashCons<'a>>
      val Obj : Object
      
      new() = 
         { UnqTable = Dictionary()
           CurrIndex = 1
           Obj = new Object() }
      
      member this.Count() = this.CurrIndex - 1
      member this.Hashcons(x : 'a) : HashCons<'a> = 
         lock obj (fun () -> 
            let b, value = this.UnqTable.TryGetValue(x)
            if b then value
            else 
               let idx = this.CurrIndex
               let hconsNode = HashCons(idx, x)
               this.UnqTable.[x] <- hconsNode
               this.CurrIndex <- this.CurrIndex + 1
               hconsNode)
   end

let private memoizeAux sz f = 
   if sz <= 0 then failwith (sprintf "Invalid cache size: %d" sz)
   let unq = Dictionary()
   (fun x -> 
   let b, value = unq.TryGetValue(x)
   if b then value
   else 
      let res = f x
      if unq.Count > sz then unq.Clear()
      unq.[x] <- res
      res)

/// Memoize a function f by caching [args -> result]
/// Arguments must be tupled to generalize.
/// Note: the function f must be immutable
let memoize f = memoizeAux CACHE_SIZE f