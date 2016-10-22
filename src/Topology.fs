module Topology

open FSharp.Data
open QuickGraph
open QuickGraph.Algorithms
open Route
open System.Collections.Generic
open System.Xml
open Util.Error
open Util.Format
open Util.String

type NodeType = 
   | Start
   | End
   | Outside
   | Inside
   | Unknown of Set<string>

type Node = 
   struct
      val Loc : string
      val Typ : NodeType
      new(l, t) = 
         { Loc = l
           Typ = t }
   end

type T = 
   | Topology of BidirectionalGraph<Node, Edge<Node>>

let copyTopology (Topology(topo) : T) : T = 
   let newTopo = BidirectionalGraph<Node, Edge<Node>>()
   for v in topo.Vertices do
      newTopo.AddVertex v |> ignore
   for e in topo.Edges do
      newTopo.AddEdge e |> ignore
   Topology(newTopo)

let alphabet (Topology(topo) : T) : Set<Node> * Set<Node> = 
   let mutable ain = Set.empty
   let mutable aout = Set.empty
   for v in topo.Vertices do
      match v.Typ with
      | Inside -> ain <- Set.add v ain
      | Outside | Unknown _ -> aout <- Set.add v aout
      | Start | End -> failwith "unreachable"
   (ain, aout)

let vertices (Topology(topo) : T) = topo.Vertices
let neighbors (Topology(topo) : T) v = topo.OutEdges v |> Seq.map (fun e -> e.Target)
let edges (Topology(topo) : T) = topo.Edges |> Seq.map (fun e -> (e.Source, e.Target))
let inEdges (Topology(topo) : T) v = topo.InEdges v |> Seq.map (fun e -> (e.Source, e.Target))
let outEdges (Topology(topo) : T) v = topo.OutEdges v |> Seq.map (fun e -> (e.Source, e.Target))

let isTopoNode (t : Node) = 
   match t.Typ with
   | Start | End -> false
   | _ -> true

let isOutside (t : Node) = 
   match t.Typ with
   | Outside -> true
   | Unknown _ -> true
   | _ -> false

let isInside (t : Node) = 
   match t.Typ with
   | Inside -> true
   | _ -> false

let isUnknown (t : Node) = 
   match t.Typ with
   | Unknown _ -> true
   | _ -> false

let canOriginateTraffic (t : Node) = 
   match t.Typ with
   | Outside -> true
   | Unknown _ -> true
   | Inside -> true
   | Start | End -> false

let isWellFormed (topo : T) : bool = 
   let (Topology(onlyInside)) = copyTopology topo
   onlyInside.RemoveVertexIf(fun v -> isOutside v) |> ignore
   let d = Dictionary<Node, int>()
   ignore (onlyInside.WeaklyConnectedComponents d)
   (Set.ofSeq d.Values).Count = 1

let rec addVertices (topo : T) (vs : Node list) = 
   let (Topology(t)) = topo
   match vs with
   | [] -> ()
   | v :: vs -> 
      t.AddVertex v |> ignore
      addVertices topo vs

let rec addEdgesUndirected (topo : T) (es : (Node * Node) list) = 
   let (Topology(t)) = topo
   match es with
   | [] -> ()
   | (x, y) :: es -> 
      let e1 = Edge(x, y)
      let e2 = Edge(y, x)
      ignore (t.AddEdge e1)
      ignore (t.AddEdge e2)
      addEdgesUndirected topo es

let rec addEdgesDirected (topo : T) (es : (Node * Node) list) = 
   let (Topology(t)) = topo
   match es with
   | [] -> ()
   | (x, y) :: es -> 
      let e = Edge(x, y)
      ignore (t.AddEdge e)
      addEdgesDirected topo es

let findByLoc (Topology(topo) : T) loc = topo.Vertices |> Seq.tryFind (fun v -> v.Loc = loc)
let peers (Topology(topo) : T) (node : Node) = topo.OutEdges node |> Seq.map (fun e -> e.Target)

let findLinks (topo : T) (froms, tos) = 
   let (Topology(t)) = topo
   let mutable pairs = []
   for x in Set.toSeq froms do
      for y in Set.toSeq tos do
         let a = findByLoc topo x
         let b = findByLoc topo y
         match a, b with
         | Some a, Some b -> 
            let ns = 
               t.OutEdges a
               |> Seq.map (fun (e : Edge<Node>) -> e.Target)
               |> Set.ofSeq
            if Set.contains b ns then pairs <- (a, b) :: pairs
         | _, _ -> ()
   pairs

type Topo = XmlProvider< "../data/template.xml" >

type Kind = 
   | Concrete
   | Abstract
   | Template

type GraphInfo = 
   { Graph : T
     Pods : Map<string, Set<string>>
     InternalNames : Set<string>
     ExternalNames : Set<string>
     AsnMap : Map<string, string>
     AsnRevMap : Map<string, string>
     RouterMap : Map<string, string>
     IpMap : Dictionary<string * string, string * string> }

type CustomLabel = 
   | SomeLabel of string
   | AllLabel of string
   | NameLabel of string

type EdgeInfo = 
   { Label : string
     OtherLabel : string option
     Source : string
     Target : string
     Scope : string
     Front : CustomLabel list
     Back : CustomLabel list }

type EdgeLabelInfo = Map<string * string, EdgeInfo list>

type TopoInfo = 
   class
      val NetworkAsn : int
      val Kind : Kind
      val ConcreteGraphInfo : GraphInfo
      val AbstractGraphInfo : GraphInfo
      val EdgeLabels : EdgeLabelInfo
      val NodeLabels : Map<string, string>
      val PodLabels : Set<string>
      val EnclosingScopes : Map<string, string list>
      val Concretization : Map<string, Set<string>>
      val Abstraction : Map<string, string>
      val Constraints : List<string>
      val TemplateVars : Map<string option * string, Set<int * int * int * int * int>>
      
      new(nasn, k, cg, ag, els, nls, pls, escopes, con, abs, cs, tvs) = 
         { NetworkAsn = nasn
           Kind = k
           ConcreteGraphInfo = cg
           AbstractGraphInfo = ag
           EdgeLabels = els
           NodeLabels = nls
           PodLabels = pls
           EnclosingScopes = escopes
           Concretization = con
           Abstraction = abs
           Constraints = cs
           TemplateVars = tvs }
      
      member this.SelectGraphInfo = 
         match this.Kind with
         | Concrete -> this.ConcreteGraphInfo
         | Abstract | Template -> this.AbstractGraphInfo
      
      member this.IsTemplate = 
         match this.Kind with
         | Concrete -> false
         | Abstract -> false
         | Template -> true
   end

let MAXASN = 65534
let counter = ref 0
let currASN = ref (MAXASN + 1)

let parseAsn s = 
   if s = "" then error (sprintf "Invalid topology: No asn specified")
   try 
      int s
   with _ -> error (sprintf "Invalid topology: Unrecognized AS number %s" s)

let getAsn isAbstract inside name (asn : string) = 
   if isAbstract then 
      if asn <> "" then 
         error (sprintf "Invalid topology: ASN included in abstract topology: %s" asn)
      else 
         incr counter
         !counter
   else if asn = "" then 
      if not inside then error (sprintf "Invalid topology: ASN required for external peer: %s" name)
      else 
         decr currASN
         !currASN
   else 
      let i = parseAsn asn
      if i < 0 then error (sprintf "Negative AS number '%s' in topology for node '%s'" asn name)
      else i

let routerMap (routerMap : Map<string, string>) (asn : string) = 
   match Map.tryFind asn routerMap with
   | None -> 
      if asn = "out" then asn
      else "AS" + asn
   | Some x -> x

let router (asn : string) (ti : TopoInfo) = routerMap ti.SelectGraphInfo.RouterMap asn

let assignIps sip tip ip = 
   match (sip, tip) with
   | "", "" -> 
      let (_, _, c, d) = Route.Bitwise.toDotted !ip
      let s = sprintf "10.%d.%d.1" c d
      let t = sprintf "10.%d.%d.2" c d
      incr ip
      (s, t)
   | _, "" | "", _ -> error "Missing source or target ip in topology"
   | _, _ -> (sip, tip)

let addEdge (g : BidirectionalGraph<Node, Edge<Node>>) (seen : HashSet<_>) x y = 
   if not (seen.Contains(x, y)) then 
      seen.Add(x, y) |> ignore
      let e = Edge(x, y)
      ignore (g.AddEdge e)

let addForGraph (asnMap, revAsnMap, nameMap, nodeMap, internalNames, externalNames, 
                 g : BidirectionalGraph<Node, Edge<Node>>) isAbstract intern name asn = 
   let asn = getAsn isAbstract intern name asn |> string
   match Map.tryFind name !asnMap with
   | None -> 
      asnMap := Map.add name asn !asnMap
      revAsnMap := Map.add (string asn) name !revAsnMap
      match Map.tryFind asn !nameMap with
      | Some e -> error (sprintf "Duplicate AS numbers for %s and %s" e name)
      | None -> ()
      nameMap := Map.add asn name !nameMap
   | Some _ -> error (sprintf "Duplicate name '%s' in topology" name)
   let typ = 
      if intern then Inside
      else Outside
   if intern then internalNames := Set.add name !internalNames
   else externalNames := Set.add name !externalNames
   let state = Node(string asn, typ)
   nodeMap := Map.add name state !nodeMap
   ignore (g.AddVertex state)

let checkForDuplicateNames names s = 
   let bySize = Seq.groupBy id (names |> List.toSeq)
   for (label, vs) in bySize do
      if Seq.length vs > 1 then 
         let msg = sprintf "Duplicate %s (%s) found in the topology. " s label
         error msg

let checkForNonNestedScopes (pods : Map<string, Set<string>>) = 
   for kv1 in pods do
      for kv2 in pods do
         if kv1.Key <> kv2.Key then 
            let xs, ys = kv1.Value, kv2.Value
            let inter = Set.intersect xs ys
            if inter.Count > 0 then 
               let both = Set.union xs ys
               if both.Count > max xs.Count ys.Count then 
                  error (sprintf "Invalid topology, overlapping pods: %s and %s" kv1.Key kv2.Key)

(* 
// Ensure custom "scope" labels are a common parent for both
let checkForValidScopes (edgeScopes : Map<string * string, string>) invMap enclosing = 
   let rec isParent x xs = 
      match xs with
      | [] -> false
      | hd :: tl -> 
         if hd = x then true
         else isParent x tl
   for kv in edgeScopes do
      let (x, y) = kv.Key
      let (x, y) = Map.find (x,y) invMap
      let label = kv.Value
      let scopex = Map.find x enclosing
      let scopey = Map.find y enclosing
      if not (isParent label scopex && isParent label scopey) then 
         error 
            (sprintf "Invalid scope: %s. Must be an enclosing scope for both source and target." 
                label) *)

let getEnclosingScopes (all : Set<string>) (pods : Map<string, Set<string>>) : Map<string, string list> = 
   let mutable acc = Map.empty
   for r in all do
      acc <- Map.add r [ "global" ] acc
   for kv in pods do
      let label = kv.Key
      let routers = kv.Value
      for r in routers do
         let e = Map.find r acc
         acc <- Map.add r (label :: e) acc
   acc

let parseLabel (v : string) = 
   let v = v.Trim()
   match sscanf "all(%s)" v with
   | Some x -> AllLabel x
   | None -> 
      match sscanf "some(%s)" v with
      | Some x -> SomeLabel x
      | None -> NameLabel v

let parseCustomLabels (ls : string []) = 
   let split vs = 
      let mutable seen = None
      let mutable front = []
      let mutable back = []
      for v in vs do
         match v, seen with
         | NameLabel _, Some y -> error (sprintf "Invalid custom label. Multiple labels")
         | NameLabel x, None -> seen <- Some x
         | AllLabel x, None | SomeLabel x, None -> front <- v :: front
         | AllLabel x, Some _ | SomeLabel x, Some _ -> back <- v :: back
      match front, back with
      | [], _ | _, [] -> error (sprintf "Invalid custom label expression")
      | _, _ -> ()
      match seen with
      | None -> error (sprintf "No label specified in custom label")
      | Some y -> (y, None, front, back)
   
   let mutable acc = []
   for l in ls do
      let vs = l.Split(',') |> Array.map parseLabel
      acc <- (split vs) :: acc
   acc

let labelName x = 
   match x with
   | AllLabel l | SomeLabel l | NameLabel l -> l

let checkValidNesting ls scopes = 
   //printfn "ls: %A" ls 
   //printfn "scopes: %A" scopes
   let mutable idx = -1
   for label in ls do
      match label with
      | AllLabel x | SomeLabel x -> 
         //printfn "  found label: %A" x
         let i = List.findIndex ((=) x) scopes
         //printfn "  got index : %d" i
         if i <= idx then error (sprintf "Invalid scope ordering in custom label for %s." x)
         idx <- i
      | _ -> failwith "unreachable"

let rec mostRecentAncestor vs us = 
   match vs with
   | [] -> failwith "unreachable"
   | hd :: tl -> 
      if List.contains hd us then hd
      else mostRecentAncestor tl us

let rec takeAfter (vs : string list) (s : string) = 
   match vs with
   | [] -> []
   | hd :: tl -> 
      if s = hd then tl
      else takeAfter tl s

let lastRelevant (labels : CustomLabel list) = 
   let rec aux last ls = 
      match ls with
      | [] -> last
      | hd :: tl -> 
         match hd with
         | SomeLabel x -> aux (Some x) tl
         | _ -> aux last tl
   aux None labels

let getLeastScope (front, back) (es, et) (scopesA, scopesB) = 
   let startB = List.head (List.rev back) |> labelName
   
   let startA = 
      match lastRelevant front with
      | None -> List.head front |> labelName
      | Some x -> x
   
   let vs = takeAfter scopesA startA
   let us = takeAfter scopesB startB
   let ret = mostRecentAncestor vs us
   (* printfn "get least scope"
   printfn "  front: %A" front 
   printfn "  back: %A" back 
   printfn "  (es,et): (%s,%s)" es et 
   printfn "  scopesA: %A" scopesA
   printfn "  scopesB: %A" scopesB
   printfn "  startA: %A" startA
   printfn "  startB: %A" startB
   printfn "  vs: %A" vs
   printfn "  us: %A" us
   printfn "  ret: %A" ret *)
   mostRecentAncestor vs us

let checkWellFormedLabels (front, back) (es, et) escopes = 
   let scopesA = es :: (Map.find es escopes)
   let scopesB = et :: (Map.find et escopes)
   let fstA = labelName (List.head front)
   let fstB = labelName (List.head back)
   if fstA = es && fstB = et then 
      checkValidNesting front scopesA
      checkValidNesting back scopesB
      let scope = getLeastScope (front, back) (es, et) (scopesA, scopesB)
      (true, fstA = fstB, scope)
   else if fstA = et && fstB = es then 
      checkValidNesting front scopesB
      checkValidNesting back scopesA
      let scope = getLeastScope (front, back) (et, es) (scopesB, scopesA)
      (false, fstA = fstB, scope)
   else error (sprintf "Invalid end points: (%s,%s)" fstA fstB)

let checkWellFormedPrefix (v, a, b, c, d, s) = 
   if a > 255 || b > 255 || c > 255 || d > 255 || s > 32 then 
      error (sprintf "Invalid prefix: %s=%u.%u.%u.%u/%u" v a b c d s)

let addTemplateVar templateVarMap key var = 
   match sscanf "%s=%u.%u.%u.%u/%u" var with
   | None -> ()
   | Some(v, a, b, c, d, s) -> 
      checkWellFormedPrefix (v, a, b, c, d, s)
      templateVarMap := Util.Map.adjust (key, v) Set.empty (Set.add (a, b, c, d, s)) !templateVarMap

let readTopology (file : string) : TopoInfo * Args.T = 
   let settings = Args.getSettings()
   try 
      let concreteSeen = HashSet() // avoid adding edges twice
      let abstractSeen = HashSet()
      let topo = Topo.Load file
      let concreteG = BidirectionalGraph<Node, Edge<Node>>()
      let abstractG = BidirectionalGraph<Node, Edge<Node>>()
      let concretePods = ref Map.empty
      let abstractPods = ref Map.empty
      let concreteNodeMap = ref Map.empty
      let abstractNodeMap = ref Map.empty
      let concreteAsnMap = ref Map.empty
      let abstractAsnMap = ref Map.empty
      let concreteRevAsnMap = ref Map.empty
      let abstractRevAsnMap = ref Map.empty
      let concreteInternalNames = ref Set.empty
      let abstractInternalNames = ref Set.empty
      let concreteExternalNames = ref Set.empty
      let abstractExternalNames = ref Set.empty
      let concreteNameMap = ref Map.empty
      let abstractNameMap = ref Map.empty
      let concreteIpMap = Dictionary()
      let abstractIpMap = Dictionary()
      let abstractNodeLabels = ref Map.empty
      let abstractPodLabels = ref Set.empty
      let abstractEdgeLabelInfo : EdgeLabelInfo ref = ref Map.empty
      let concreteNames = ref []
      let abstractNames = ref [ "global" ] // reserve name for outer-most scope
      let concretization = ref Map.empty
      let abstraction = ref Map.empty
      let templateVarMap = ref Map.empty
      let currASN = ref MAXASN
      // collect constraints
      let constraints = List()
      for c in topo.Constraints do
         constraints.Add c.Assertion
      // Add the global template variables
      for v in topo.Datas do
         addTemplateVar templateVarMap None v.Vars
      // Build the concretization map and determine if we are using an abstract topology
      let isPureAbstract = (topo.Nodes.Length = 0)
      let mutable isAbstract = isPureAbstract
      for an in topo.Abstractnodes do
         concretization := Map.add an.Label Set.empty !concretization
      for n in topo.Nodes do
         if n.Group <> "" then 
            isAbstract <- true
            abstraction := Map.add n.Name n.Group !abstraction
            match Map.tryFind n.Group !concretization with
            | None -> 
               error (sprintf "Concrete node: %s refers to non-existant group %s" n.Name n.Group)
            | Some s -> 
               let s' = Set.add n.Name s
               concretization := Map.add n.Group s' !concretization
            match n.Vars with
            | None -> ()
            | Some s -> 
               let all = s.Split(',') |> Array.map (fun s -> s.Trim())
               for var in all do
                  addTemplateVar templateVarMap (Some n.Name) var
      // Add the pods to the graph
      for p in topo.Abstractpods do
         if p.Label = "" then error (sprintf "Invalid abstract pod label - found empty string")
         abstractPods := Map.add p.Label (p.Elements |> Set.ofArray) !abstractPods
         abstractPodLabels := Set.add p.Label !abstractPodLabels
         abstractNames := p.Label :: !abstractNames
      for p in topo.Pods do
         if p.Name = "" then error (sprintf "Invalid concrete pod name - found empty string")
         match Map.tryFind p.Group !abstractPods with
         | None -> error (sprintf "Invalid abstract pod group: %s" p.Group)
         | Some _ -> 
            concretePods := Map.add p.Name (p.Elements |> Set.ofArray) !concretePods
            concreteNames := p.Name :: !concreteNames
      // Ensure pods do not overlap (i.e., always contain a strict superset of the nodes)
      checkForNonNestedScopes !concretePods
      checkForNonNestedScopes !abstractPods
      // Add nodes to the concrete graph
      for n in topo.Nodes do
         concreteNames := n.Name :: !concreteNames
         let args = 
            (concreteAsnMap, concreteRevAsnMap, concreteNameMap, concreteNodeMap, 
             concreteInternalNames, concreteExternalNames, concreteG)
         addForGraph args false n.Internal n.Name n.Asn
      // Addo nodes to the abstract graph
      if isAbstract then 
         for n in topo.Abstractnodes do
            abstractNames := n.Label :: !abstractNames
            if n.Label = "" then error (sprintf "Invalid abstract node label - found empty string")
            abstractNodeLabels := Map.add n.Label n.Label !abstractNodeLabels
            let args = 
               (abstractAsnMap, abstractRevAsnMap, abstractNameMap, abstractNodeMap, 
                abstractInternalNames, abstractExternalNames, abstractG)
            addForGraph args true n.Internal n.Label ""
      // Compute enclosing scopes and check for well-formedness
      let escopes = 
         getEnclosingScopes (Set.union !abstractInternalNames !abstractExternalNames) !abstractPods
      // Perform IP address asssignment and add edges to graph
      let ip = ref 0
      for e in topo.Edges do
         if not ((!concreteNodeMap).ContainsKey e.Source) then 
            error (sprintf "Invalid edge source location %s in topology" e.Source)
         elif not ((!concreteNodeMap).ContainsKey e.Target) then 
            error (sprintf "Invalid edge target location %s in topology" e.Target)
         else 
            let x = (!concreteNodeMap).[e.Source]
            let y = (!concreteNodeMap).[e.Target]
            addEdge concreteG concreteSeen x y
            addEdge concreteG concreteSeen y x
            let (s, t) = assignIps e.SourceIp e.TargetIp ip
            concreteIpMap.[(x.Loc, y.Loc)] <- (s, t)
            concreteIpMap.[(y.Loc, x.Loc)] <- (t, s)
      // Add abstract edges if applicable
      if isAbstract then 
         for e in topo.Abstractedges do
            if not ((!abstractNodeMap).ContainsKey e.Source) then 
               error (sprintf "Invalid abstract edge source location %s in topology" e.Source)
            elif not ((!abstractNodeMap).ContainsKey e.Target) then 
               error (sprintf "Invalid abstract edge target location %s in topology" e.Target)
            else 
               let (es, et) = e.Source, e.Target
               let mutable cls = parseCustomLabels e.CustomLabels
               match sscanf "(%s,%s)" e.Labels with
               | None -> ()
               | Some(sl, tl) -> 
                  if es = et && sl <> tl then 
                     error (sprintf "Self loop for %s must contain identical edge labels" e.Source)
                  cls <- (sl, Some tl, [ AllLabel es ], [ SomeLabel et ]) :: cls
                  if es <> et then cls <- (tl, Some sl, [ AllLabel et ], [ SomeLabel es ]) :: cls
               let mutable edgeInfo = []
               for info in cls do
                  let (label, otherLabel, front, back) = info
                  let isSource, eqLabels, scope = 
                     checkWellFormedLabels (front, back) (es, et) escopes
                  
                  let v = 
                     { Label = label
                       OtherLabel = 
                          if eqLabels then Some label
                          else otherLabel
                       Source = 
                          if isSource then es
                          else et
                       Target = 
                          if isSource then et
                          else es
                       Scope = scope
                       Front = front
                       Back = back }
                  edgeInfo <- v :: edgeInfo
                  abstractNames := label :: !abstractNames
                  let x = (!abstractNodeMap).[es]
                  let y = (!abstractNodeMap).[et]
                  addEdge abstractG abstractSeen x y
                  addEdge abstractG abstractSeen y x
               abstractEdgeLabelInfo := Map.add (es, et) edgeInfo !abstractEdgeLabelInfo
               abstractEdgeLabelInfo := Map.add (et, es) edgeInfo !abstractEdgeLabelInfo
      // Check for duplicate names
      checkForDuplicateNames !abstractNames "label"
      checkForDuplicateNames !concreteNames "name"
      // Ensure abstract preserves internal/external nodes
      for kv in !abstraction do
         let a = Set.contains kv.Value !abstractInternalNames
         let b = Set.contains kv.Key !concreteInternalNames
         if (a && not b) || (b && not a) then 
            let astr = 
               if a then "internal"
               else "external"
            
            let bstr = 
               if b then "internal"
               else "external"
            
            let msg = 
               sprintf "Concrete location %s is %s, but maps to abstract location %s, which is %s" 
                  kv.Key astr kv.Value bstr
            error msg
      // Get the kind of compilation to perform
      let kind = 
         if isPureAbstract then Template
         else if isAbstract then Abstract
         else Concrete
      // Warn if using a template that certain checks can't be done
      if isPureAbstract then 
         let msg = 
            sprintf 
               "The topology provided is purely abstract. This means that the safety checks will only hold " 
            + sprintf "if the concrete topology is a true instantiation of the abstract topology."
         warning msg
      // Update the settings so we know if we are compiling an abstract topology
      Args.changeSettings { settings with IsAbstract = isAbstract
                                          IsTemplate = isPureAbstract }
      let concreteGI = 
         { Graph = Topology(concreteG)
           Pods = !concretePods
           InternalNames = !concreteInternalNames
           ExternalNames = !concreteExternalNames
           AsnMap = !concreteAsnMap
           AsnRevMap = !concreteRevAsnMap
           RouterMap = !concreteRevAsnMap
           IpMap = concreteIpMap }
      
      let abstractGI = 
         { Graph = Topology(abstractG)
           Pods = !abstractPods
           InternalNames = !abstractInternalNames
           ExternalNames = !abstractExternalNames
           AsnMap = !abstractAsnMap
           AsnRevMap = !abstractRevAsnMap
           RouterMap = !abstractRevAsnMap
           IpMap = abstractIpMap }
      
      let netAsn = parseAsn (topo.Asn)
      let ti = 
         TopoInfo
            (netAsn, kind, concreteGI, abstractGI, !abstractEdgeLabelInfo, !abstractNodeLabels, 
             !abstractPodLabels, escopes, !concretization, !abstraction, constraints, 
             !templateVarMap)
      (ti, Args.getSettings())
   with _ -> error (sprintf "Invalid topology XML file")

module Examples = 
   let topoDisconnected() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      addVertices g [ vA; vB; vC; vD ]
      addEdgesUndirected g [ (vA, vB)
                             (vC, vD) ]
      g
   
   let topoDiamond() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vX = Node("X", Inside)
      let vM = Node("M", Inside)
      let vN = Node("N", Inside)
      let vY = Node("Y", Inside)
      let vZ = Node("Z", Inside)
      let vB = Node("B", Inside)
      addVertices g [ vA; vX; vM; vN; vY; vZ; vB ]
      addEdgesUndirected g [ (vA, vX)
                             (vA, vM)
                             (vM, vN)
                             (vX, vN)
                             (vN, vY)
                             (vN, vZ)
                             (vY, vB)
                             (vZ, vB) ]
      g
   
   let topoDatacenterSmall() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vX = Node("X", Inside)
      let vY = Node("Y", Inside)
      let vM = Node("M", Inside)
      let vN = Node("N", Inside)
      addVertices g [ vA; vB; vC; vD; vX; vY; vM; vN ]
      addEdgesUndirected g [ (vA, vX)
                             (vB, vX)
                             (vC, vY)
                             (vD, vY)
                             (vX, vM)
                             (vX, vN)
                             (vY, vM)
                             (vY, vN) ]
      g
   
   let topoDatacenterMedium() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      let vF = Node("F", Inside)
      let vG = Node("G", Inside)
      let vH = Node("H", Inside)
      let vX = Node("X", Inside)
      let vY = Node("Y", Inside)
      addVertices g [ vA; vB; vC; vD; vE; vF; vG; vH; vX; vY ]
      addEdgesUndirected g [ (vA, vC)
                             (vA, vD)
                             (vB, vC)
                             (vB, vD)
                             (vE, vG)
                             (vE, vH)
                             (vF, vG)
                             (vF, vH)
                             (vC, vX)
                             (vC, vY)
                             (vD, vX)
                             (vD, vY)
                             (vG, vX)
                             (vG, vY)
                             (vH, vX)
                             (vH, vY) ]
      g
   
   let topoDatacenterMediumAggregation() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      let vF = Node("F", Inside)
      let vG = Node("G", Inside)
      let vH = Node("H", Inside)
      let vX = Node("X", Inside)
      let vY = Node("Y", Inside)
      let vPeer = Node("PEER", Outside)
      addVertices g [ vA; vB; vC; vD; vE; vF; vG; vH; vX; vY; vPeer ]
      addEdgesUndirected g [ (vA, vC)
                             (vA, vD)
                             (vB, vC)
                             (vB, vD)
                             (vE, vG)
                             (vE, vH)
                             (vF, vG)
                             (vF, vH)
                             (vC, vX)
                             (vC, vY)
                             (vD, vX)
                             (vD, vY)
                             (vG, vX)
                             (vG, vY)
                             (vH, vX)
                             (vH, vY)
                             (vX, vPeer)
                             (vY, vPeer) ]
      g
   
   let topoDatacenterLarge() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      let vF = Node("F", Inside)
      let vM = Node("M", Inside)
      let vN = Node("N", Inside)
      let vO = Node("O", Inside)
      let vX = Node("X", Inside)
      let vY = Node("Y", Inside)
      let vZ = Node("Z", Inside)
      addVertices g [ vA; vB; vC; vD; vE; vF; vM; vN; vO; vX; vY; vZ ]
      addEdgesUndirected g [ (vA, vX)
                             (vB, vX)
                             (vC, vY)
                             (vD, vY)
                             (vE, vZ)
                             (vF, vZ)
                             (vX, vM)
                             (vX, vN)
                             (vX, vO)
                             (vY, vM)
                             (vY, vN)
                             (vY, vO)
                             (vZ, vM)
                             (vZ, vN)
                             (vZ, vO) ]
      g
   
   let topoBadGadget() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      addVertices g [ vA; vB; vC; vD ]
      addEdgesUndirected g [ (vA, vB)
                             (vB, vC)
                             (vC, vA)
                             (vA, vD)
                             (vB, vD)
                             (vC, vD) ]
      g
   
   let topoBrokenTriangle() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      addVertices g [ vA; vB; vC; vD; vE ]
      addEdgesUndirected g [ (vC, vA)
                             (vA, vE)
                             (vA, vB)
                             (vE, vD)
                             (vD, vB) ]
      g
   
   let topoBigDipper() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      addVertices g [ vA; vC; vD; vE ]
      addEdgesUndirected g [ (vC, vA)
                             (vA, vE)
                             (vA, vD)
                             (vE, vD) ]
      g
   
   let topoSeesaw() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vM = Node("M", Inside)
      let vN = Node("N", Inside)
      let vO = Node("O", Inside)
      let vX = Node("X", Inside)
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      addVertices g [ vM; vN; vO; vX; vA; vB ]
      addEdgesUndirected g [ (vM, vN)
                             (vM, vO)
                             (vO, vX)
                             (vN, vX)
                             (vX, vA)
                             (vX, vB) ]
      g
   
   let topoStretchingManWAN() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vX = Node("X", Outside)
      let vY = Node("Y", Outside)
      let vZ = Node("Z", Outside)
      addVertices g [ vA; vB; vC; vD; vX; vY; vZ ]
      addEdgesUndirected g [ (vX, vA)
                             (vX, vB)
                             (vA, vC)
                             (vB, vC)
                             (vC, vD)
                             (vD, vY)
                             (vD, vZ) ]
      g
   
   let topoStretchingManWAN2() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      let vW = Node("W", Outside)
      let vX = Node("X", Outside)
      let vY = Node("Y", Outside)
      let vZ = Node("Z", Outside)
      addVertices g [ vA; vB; vC; vD; vE; vW; vX; vY; vZ ]
      addEdgesUndirected g [ (vW, vA)
                             (vW, vB)
                             (vA, vC)
                             (vB, vC)
                             (vC, vD)
                             (vC, vE)
                             (vD, vX)
                             (vD, vY)
                             (vE, vZ) ]
      g
   
   let topoPinCushionWAN() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vB = Node("B", Inside)
      let vC = Node("C", Inside)
      let vD = Node("D", Inside)
      let vE = Node("E", Inside)
      let vW = Node("W", Outside)
      let vX = Node("X", Outside)
      let vY = Node("Y", Outside)
      let vZ = Node("Z", Outside)
      addVertices g [ vA; vB; vC; vD; vE; vW; vX; vY; vZ ]
      addEdgesUndirected g [ (vW, vA)
                             (vX, vB)
                             (vA, vC)
                             (vB, vC)
                             (vC, vD)
                             (vC, vE)
                             (vD, vY)
                             (vE, vZ) ]
      g
   
   let topoBackboneWAN() = 
      let g = Topology(BidirectionalGraph<Node, Edge<Node>>())
      let vA = Node("A", Inside)
      let vSEA = Node("SEA", Inside)
      let vNY = Node("NY", Inside)
      let vX = Node("X", Outside)
      let vY = Node("Y", Outside)
      addVertices g [ vA; vSEA; vNY; vX; vY ]
      addEdgesUndirected g [ (vA, vSEA)
                             (vA, vNY)
                             (vSEA, vX)
                             (vNY, vY) ]
      g
   
   /// Fattree topology 
   type Tiers = Dictionary<Node, int>
   
   type Prefixes = Dictionary<Node, Prefix>
   
   let getPrefix i = 
      let (a, b, c, d) = Route.Bitwise.toDotted i
      Prefix(a, b, c, d, 32)
   
   let fatTree k : T * Prefixes * Tiers = 
      let iT0 = (k * k) / 2
      let iT1 = (k * k) / 2
      let iT2 = (k * k) / 4
      let g = BidirectionalGraph<Node, Edge<Node>>()
      let prefixes = Dictionary()
      let tiers = Dictionary()
      
      let routersT0 = 
         Array.init iT0 (fun i -> 
            let name = "T0_" + string i
            let v = Node(name, Inside)
            ignore (g.AddVertex v)
            prefixes.[v] <- getPrefix i
            tiers.[v] <- 0
            v)
      
      let routersT1 = 
         Array.init iT1 (fun i -> 
            let name = "T1_" + string i
            let v = Node(name, Inside)
            ignore (g.AddVertex v)
            tiers.[v] <- 1
            v)
      
      let routersT2 = 
         Array.init iT2 (fun i -> 
            let name = "T2_" + string i
            let v = Node(name, Inside)
            ignore (g.AddVertex v)
            tiers.[v] <- 2
            v)
      
      let t = Topology(g)
      let perPod = (k / 2)
      for i = 0 to iT0 - 1 do
         let pod = i / (perPod)
         for j = 0 to perPod - 1 do
            let x = routersT0.[i]
            let y = routersT1.[pod * perPod + j]
            addEdgesUndirected t [ (x, y) ]
      for i = 0 to iT1 - 1 do
         for j = 0 to perPod - 1 do
            let rem = i % perPod
            let x = routersT1.[i]
            let y = routersT2.[rem * perPod + j]
            addEdgesUndirected t [ (x, y) ]
      let back1 = Node("BACK1", Outside)
      let back2 = Node("BACK2", Outside)
      ignore (g.AddVertex back1)
      ignore (g.AddVertex back2)
      for i = 0 to iT2 - 1 do
         let x = routersT2.[i]
         addEdgesUndirected t [ (x, back1)
                                (x, back2) ]
      (t, prefixes, tiers)
   
   let complete n = 
      let g = BidirectionalGraph<Node, Edge<Node>>()
      // setup external peers
      let externalPeers = Dictionary()
      let internalPeers = Dictionary()
      // setup internal full mesh
      for i = 0 to n - 1 do
         let name = "R" + string i
         let v = Node(name, Inside)
         ignore (g.AddVertex v)
         internalPeers.[name] <- v
      for v1 in g.Vertices do
         for v2 in g.Vertices do
            if isInside v1 && isInside v2 && v1 <> v2 then 
               let e = Edge(v1, v2)
               ignore (g.AddEdge e)
      // add connections to external peers
      for kv in internalPeers do
         let name = kv.Key
         let router = kv.Value
         // add dcs
         for i = 0 to 9 do
            let eName = "Cust" + string i + name
            let ePeer = Node(eName, Outside)
            ignore (g.AddVertex ePeer)
            let e1 = Edge(router, ePeer)
            let e2 = Edge(ePeer, router)
            ignore (g.AddEdge e1)
            ignore (g.AddEdge e2)
         // add peers
         for i = 0 to 19 do
            let eName = "Peer" + string i + name
            let ePeer = Node(eName, Outside)
            ignore (g.AddVertex ePeer)
            let e1 = Edge(router, ePeer)
            let e2 = Edge(ePeer, router)
            ignore (g.AddEdge e1)
            ignore (g.AddEdge e2)
         // add paid on net
         for i = 0 to 19 do
            let eName = "OnPaid" + string i + name
            let ePeer = Node(eName, Outside)
            ignore (g.AddVertex ePeer)
            let e1 = Edge(router, ePeer)
            let e2 = Edge(ePeer, router)
            ignore (g.AddEdge e1)
            ignore (g.AddEdge e2)
         // add paid off net
         for i = 0 to 19 do
            let eName = "OffPaid" + string i + name
            let ePeer = Node(eName, Outside)
            ignore (g.AddVertex ePeer)
            let e1 = Edge(router, ePeer)
            let e2 = Edge(ePeer, router)
            ignore (g.AddEdge e1)
            ignore (g.AddEdge e2)
      Topology(g)

module Test = 
   let testTopologyWellFormedness() = 
      printf "Topology well-formedness "
      let topo = Examples.topoDisconnected()
      if isWellFormed topo then failed()
      else passed()
   
   let run() = testTopologyWellFormedness()