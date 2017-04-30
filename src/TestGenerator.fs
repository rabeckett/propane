module TestGenerator

open CGraph
open Microsoft.Z3
open Topology
open System
open System.Net
open System.IO
open System.Collections.Generic
open Util
open Util.Debug
open Util.Error
open Util.Format

// set of edges where each edge is a pair of vertices
type Path = Set<CgState*CgState> 
type TestCases = Set<Path*Path>

type T = 
    {
        routerNameToIpMap: Map<String, String>
        predToTestCases: Map<Route.Predicate, TestCases>
    }

// take an integer value and convert it into an ipAddress
// used to generate the ipaddress for individual nodes in cbgp
let ipOfInt (d : uint32) =
    BitConverter.GetBytes d
    |> Array.rev
    |> IPAddress
    |> string

let generateRouterIp topo : Map<string, string> =
    let mutable routerToIpMap = Map.empty
    let vertices = Topology.vertices topo in
    for i in 0 .. (Seq.length vertices - 1) do
        let vertex = Seq.item i vertices 
        let routerName = vertex.Loc 
        //Topology.router vertex.Loc topoInfo |> ignore
        routerToIpMap <- Map.add routerName (ipOfInt (uint32 (i + 1))) routerToIpMap
    routerToIpMap

let geteBGPStaticRoutes (vertexToPeers : Map<Topology.Node, Set<Topology.Node>>) (routerNameToIp : Map<string, string>) file : unit =
    let printSingleRouter (router : Topology.Node) (neighbors : Set<Topology.Node>) =
        if not (Seq.isEmpty neighbors) then
            for n in neighbors do
                let peerNum = n.Loc
                if (not(Map.containsKey peerNum routerNameToIp)) then Console.Write("missing this" + peerNum);
                let peerIp = Map.find peerNum routerNameToIp
                if (not(Map.containsKey router.Loc routerNameToIp)) then Console.Write("missing this me" + router.Loc);
                let routerIp = Map.find router.Loc routerNameToIp
                let nodeConnect = "\nnet node " + routerIp + " route add " + peerIp + "/32 --oif=" + peerIp + " 1"
                File.AppendAllText(file, nodeConnect);
        else ();               
    Map.iter printSingleRouter vertexToPeers
    File.AppendAllText(file, "\n\n"); 

// writes the physical topology in CBGP format 
let writeTopoCBGP (input : Topology.T) (file : string) : unit = 
    let vertices = Topology.vertices input in
    let mutable vMap = Map.empty in
    File.WriteAllText(file, ""); // empty the file

    // create nodes for the vertices
    for i in 0 .. (Seq.length vertices - 1) do
        let vertex = Seq.item i vertices in
        vMap <- Map.add vertex (i + 1) vMap;
        //if (not (Topology.isUnknown vertex)) then 
        let toWrite = "net add node " + ipOfInt ((uint32) (i + 1)) in
        File.AppendAllText(file, toWrite + "\n");

    //create links for the edges
    let mutable edgeSet = Set.ofSeq (Topology.edges input)
    for e in edgeSet do
        let (src, target) = e
        if (Set.contains (target, src) edgeSet) then
         edgeSet <- Set.remove (target, src) edgeSet
        let srcIdx = Map.find src vMap
        let targetIdx = Map.find target vMap
        if (srcIdx < targetIdx) then // &&  (not (Topology.isUnknown src)) && (not (Topology.isUnknown target)) ) then 
            let toWrite = ipOfInt ((uint32) srcIdx) + " " + ipOfInt ((uint32) targetIdx) in
            File.AppendAllText(file, "net add link " + toWrite + "\n");
        // should i be adding the bGP router/igp stuff right here -rulelessly   

// generates the link ocverage tests for the given predicate
let genLinkTest (input: CGraph.T) (coverage : int) (pred : Route.Predicate) : TestCases*float =
    let ctx = new Context() in
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in

    //Console.Write("Product graph edges");
    //for e in edges do
    //    Console.Write("(" + (string e.Source) + "," + (string e.Target) + ")");
    //    Console.Write("\n");

    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    let vIntArray = Array.zeroCreate (Seq.length vertices) in
    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in
    let mutable condSet = Set.empty in

    //cretae vertex map
    //Console.Write("creating vertex map");
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBoolConst ("v" + (string i)));
        Array.set vIntArray i (ctx.MkIntConst ("vI" + (string i)));
        vMap <- Map.add (Seq.item i vertices)  i vMap;
        let vertex = Seq.item i vertices
        if (Topology.isUnknown vertex.Node) then
            condSet <- Set.add (ctx.MkNot vArray.[i]) condSet
    let eArray = Array.zeroCreate (Seq.length edges) in

    // create edge map
    //Console.Write("creating edge map");
    for i in 0 .. (Seq.length edges - 1) do
        Array.set eArray i (ctx.MkBoolConst ("e" + (string i)));
        let edge = Seq.item i edges in
        eMap <- Map.add (edge.Source, edge.Target) i eMap;

    let isEdgeInternal (e: QuickGraph.Edge<CGraph.CgState>) = 
        not (e.Source = input.Start || e.Target = input.End)

    // generate a random set of edges to cover
    let mapEdge (e: QuickGraph.Edge<CGraph.CgState>) = eArray.[Map.find (e.Source, e.Target) eMap]
    let internalEdges = Seq.filter isEdgeInternal edges |> Seq.map mapEdge
    let origSize = Seq.length internalEdges
    let mutable edgesToCover = Set.empty
    if (coverage = 100) then
        edgesToCover <- Set.ofSeq internalEdges
    else 
        let R = System.Random()
        while (float (Set.count edgesToCover) < (float coverage) / 100.0 * (float origSize)) do 
            let newindex = R.Next(1, origSize)
            if (not (Set.contains (Seq.item newindex internalEdges) edgesToCover)) then
                edgesToCover <- Set.add (Seq.item newindex internalEdges) edgesToCover;
            else ();
    //Console.Write((string origSize) + " edges out of which covering " + (string (Set.count edgesToCover)) + "\n");
    

    let src = Map.find input.Start vMap in
    condSet <- Set.add (Array.get vArray src) condSet ;
    condSet <- Set.add (ctx.MkEq (vIntArray.[src], ctx.MkInt(0))) condSet;
    let target = Map.find input.End vMap in
    condSet <- Set.add (Array.get vArray target) condSet ;

    // if edge is true, both vertices are true, and int for source vertex is one less than dest
    //Console.Write("if edge then ends");
    for i in 0 .. (Seq.length edges - 1) do
        // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
        let edge = Seq.item i edges in
        let a = Map.find edge.Source vMap in
        let b = Map.find edge.Target vMap in
        let arr = Array.create 2 vArray.[a] in
        Array.set arr 1 vArray.[b];
        let ends = ctx.MkAnd arr in
        let exp = ctx.MkImplies (eArray.[i], ends) in
        condSet <- Set.add exp condSet;
        let srcPlusOne = ctx.MkAdd(vIntArray.[a], ctx.MkInt(1)) in
        let incValueExp = ctx.MkEq(vIntArray.[b], srcPlusOne) in
        condSet <- Set.add (ctx.MkImplies (eArray.[i], incValueExp)) condSet

    // if a vertex is true, atleast one incoming edge is true, and atleast one outgoign edge
    for j in 0 .. (Seq.length vertices - 1) do
        // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
        let vertex = Seq.item j vertices in   

        // atleast one incoming edge is true
        //Console.Write("ifvertex then incoming");
        let incoming = input.Graph.InEdges vertex in
        let arr = Array.create (Seq.length incoming) (ctx.MkTrue()) in
        for i in 0 .. (Seq.length incoming - 1) do
            let e = Seq.item i incoming in
            //Console.Write("edge is " + (string) e + "\n");
            let eVar = Map.find (e.Source, e.Target) eMap in
            //Console.Write("eVar is " + (string) eVar + "\n");
            Array.set arr i (ctx.MkNot eArray.[eVar]);
        if Seq.length incoming > 0 then
            let exp = ctx.MkAtMost(arr, ((uint32) (Seq.length incoming) - 1u)) in
            condSet <- Set.add (ctx.MkImplies (vArray.[j], exp)) condSet;
        else ();

        // exactly one outgoing edge is true
        //Console.Write("if vertex then one outgoing edge");
        let outgoing = input.Graph.OutEdges vertex in
        let arr = Array.create (Seq.length outgoing) (ctx.MkTrue()) in
        let notArr = Array.create (Seq.length outgoing) (ctx.MkTrue()) in
        for i in 0 .. (Seq.length outgoing - 1) do
            let e = Seq.item i outgoing in
            let eVar = Map.find (e.Source, e.Target) eMap in
            Array.set arr i eArray.[eVar];
            Array.set notArr i (ctx.MkNot eArray.[eVar]);
        let exp = ctx.MkAtMost(arr, 1u) in
        let combArr = Array.create 2 exp in
        let notexp =
            if Seq.length outgoing > 0 then
                ctx.MkAtMost(notArr, ((uint32) (Seq.length outgoing) - 1u))
            else 
                ctx.MkTrue() in
        Array.set combArr 1 notexp;
        condSet <- Set.add (ctx.MkImplies (vArray.[j], ctx.MkAnd(combArr))) condSet;

    // relationship between nodes with the same topological location
    // identify using CgState.Node field which gives the topo.node
    // basically put all of these variables in one set and use a mutual
    //implication statement?
    //use Start and End in CGraph, map them to topological node and use
    // them for first connectivity constraint

    //Console.Write("loopfree");
    let mutable topoNodeToVertexSet = Map.empty in
    for i in 0 .. (Seq.length vertices - 1) do
        let vertex = Seq.item i vertices in
        let vertexExp = vArray.[Map.find (Seq.item i vertices) vMap] in
        let topoNode = vertex.Node in
        if (Map.containsKey topoNode topoNodeToVertexSet) then
            let newSet = Set.add vertexExp (Map.find topoNode topoNodeToVertexSet) in
            topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;
        else
            let newSet = Set.add vertexExp Set.empty in
            topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;

    //iterate through this map, creating statements per set for topological Node 
    //Console.Write("actually adding loopfree condiitons");
    let prepCondition key value = 
        let exp = ctx.MkAtMost((Set.toArray value), 1u) in
        condSet <- Set.add exp condSet;
    in
    Map.iter prepCondition topoNodeToVertexSet;

    // make the solver and iterate through it
    let s = ctx.MkSolver();
    //let p = ctx.MkParams();
    //p.Add("timeout", (uint32 10000));
    //s.Parameters <- p;

    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()

    //Console.Write(string s.timeout);
    s.Assert(Set.toArray condSet);
    s.Push();
    let newSolnRule = ctx.MkOr(Set.toArray edgesToCover)
    s.Assert(Array.create 1 newSolnRule)
    //File.AppendAllText("solutions.txt", "New Set for prefix \n")
    let mutable tests = Set.empty in
    //Seq.iter (fun a -> Console.Write((string a) + "\n")) s.Assertions
    //while (timer.ElapsedMilliseconds <= (int64 30000) && s.Check() = Status.SATISFIABLE && not (Set.isEmpty edgesToCover)) do
    while (s.Check() = Status.SATISFIABLE && not (Set.isEmpty edgesToCover)) do
      let mutable solnSet = Set.empty in
      let mutable curPath = Set.empty in
      //let mutable isOutPath = false in
      //File.AppendAllText("solutions.txt", "New Solution\n")
      for i in 0 .. (Seq.length edges - 1) do
        if (s.Model.ConstInterp(eArray.[i]).IsTrue) then
            solnSet <- Set.add eArray.[i] solnSet;

            //add current edge to the current Path
            let edge = Seq.item i edges in
            if (isEdgeInternal edge && Set.contains eArray.[i] edgesToCover) then
                edgesToCover <- Set.remove eArray.[i] edgesToCover;

            curPath <- Set.add (edge.Source, edge.Target) curPath;

            //let a = s.Model.Evaluate(vIntArray.[Map.find edge.Source vMap]) in
            //let b = s.Model.Evaluate(vIntArray.[Map.find edge.Target vMap]) in
            //File.AppendAllText("solutions.txt", (string) (Seq.item i edges) + " " + (string a) + " " + (string b) + "\n");
            
            //bgp peer up? for cbgp file
        else
            ();
      if (Set.count curPath > 2) then 
        tests <- Set.add (curPath, curPath) tests;
      s.Pop()
      s.Push()
      let newSolnRule = ctx.MkOr(Set.toArray edgesToCover)
      s.Assert(Array.create 1 newSolnRule)
    if (origSize = 0) then
        (tests, -1.0)
    else
        (tests, (1.0 - (float (Set.count edgesToCover))/ (float origSize)))


// generates the individual pref tests for all nodes that share a given topological node
let getPrefIndividualProblems (input: CGraph.T) (ctx : Context) nodeSet vArray eArray vIntArray =
    let k = Seq.length nodeSet
    let sameTopoCond = Array.create k Set.empty
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in

    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in

    //Console.Write("\nNew nodes sharing location");
    //for n in nodeSet do
    //    Console.Write((string n) + " ");
    
    //create vertex map
    //Console.Write("creating vertex map");
    for i in 0 .. (Seq.length vertices - 1) do
        vMap <- Map.add (Seq.item i vertices)  i vMap;

    for i in 0 .. (Seq.length edges - 1) do
        let edge = Seq.item i edges in
        eMap <- Map.add (edge.Source, edge.Target) i eMap;

    for index in 0 .. (k - 1) do      
        let mutable condSet = Set.empty in

        // src target and current node is being set
        let src = Map.find input.Start vMap in
        condSet <- Set.add (Array2D.get vArray src index) condSet ;
        condSet <- Set.add (ctx.MkEq ((Array2D.get vIntArray src index), ctx.MkInt(0))) condSet;
        let target = Map.find input.End vMap in
        condSet <- Set.add (Array2D.get vArray target index) condSet ;
        let myIndex = Map.find (Seq.item index nodeSet) vMap in
        condSet <- Set.add (Array2D.get vArray myIndex index) condSet; 

        let mkImplicationsAcrossEdges (thisEdge : QuickGraph.Edge<CgState>)  (otherEdge : QuickGraph.Edge<CgState>) =
            let myNode = Seq.item index nodeSet
            let thisa = thisEdge.Source in
            let thisb = thisEdge.Target in
            let thisIndex = Map.find (thisa, thisb) eMap
    
            let othera = otherEdge.Source in
            let otherb = otherEdge.Target in
            let otherIndex = Map.find (othera, otherb) eMap

            if (thisa.Node = othera.Node && thisb.Node = otherb.Node && thisa.State = myNode.State) then
                for j in 0 .. (k - 1) do
                    if (j <> index) then
                        condSet <- Set.add (ctx.MkImplies ((Array2D.get eArray otherIndex j), (Array2D.get eArray thisIndex index))) condSet

    
        //Console.Write("if edge then ends");
        for i in 0 .. (Seq.length edges - 1) do
            // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
            let edge = Seq.item i edges in
            let a = Map.find edge.Source vMap in
            let b = Map.find edge.Target vMap in
            let arr = Array.create 2 (Array2D.get vArray a index) in
            Array.set arr 1 (Array2D.get vArray b index);
            let ends = ctx.MkAnd arr in
            let exp = ctx.MkImplies ((Array2D.get eArray i index), ends) in
            condSet <- Set.add exp condSet;
            let srcPlusOne = ctx.MkAdd(Array2D.get vIntArray a index, ctx.MkInt(1)) in
            let incValueExp = ctx.MkEq((Array2D.get vIntArray b index), srcPlusOne) in
            condSet <- Set.add (ctx.MkImplies ((Array2D.get eArray i index), incValueExp)) condSet

            // if edge is present in another of the k problems, it should be present her too
            for otherEdge in edges do
                let myNode = Seq.item index nodeSet
                let thisa = edge.Source in
                let thisb = edge.Target in
                let thisIndex = Map.find (thisa, thisb) eMap
        
                let othera = otherEdge.Source in
                let otherb = otherEdge.Target in
                let otherIndex = Map.find (othera, otherb) eMap

                if (thisa.Node = othera.Node && thisb.Node = otherb.Node && thisa.State = myNode.State) then
                    for j in 0 .. (k - 1) do
                        if (j <> index) then
                            condSet <- Set.add (ctx.MkImplies ((Array2D.get eArray otherIndex j), (Array2D.get eArray thisIndex index))) condSet

        // if a vertex is true, atleast one incoming edge is true, and atleast one outgoign edge
        for j in 0 .. (Seq.length vertices - 1) do
            // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
            let vertex = Seq.item j vertices in   
            if (Topology.isUnknown vertex.Node) then
                condSet <- Set.add (ctx.MkNot (Array2D.get vArray j index)) condSet

            // atleast one incoming edge is true
            //Console.Write("ifvertex then incoming");
            let incoming = input.Graph.InEdges vertex in
            let arr = Array.create (Seq.length incoming) (ctx.MkTrue()) in
            for i in 0 .. (Seq.length incoming - 1) do
                let e = Seq.item i incoming in
                //Console.Write("edge is " + (string) e + "\n");
                let eVar = Map.find (e.Source, e.Target) eMap in
                //Console.Write("eVar is " + (string) eVar + "\n");
                Array.set arr i (ctx.MkNot (Array2D.get eArray eVar index));
            if Seq.length incoming > 0 then
                let exp = ctx.MkAtMost(arr, ((uint32) (Seq.length incoming) - 1u)) in
                condSet <- Set.add (ctx.MkImplies ((Array2D.get vArray j index), exp)) condSet;
            else ();

            // exactly one outgoing edge is true
            //Console.Write("if vertex then one outgoing edge");
            let outgoing = input.Graph.OutEdges vertex in
            let arr = Array.create (Seq.length outgoing) (ctx.MkTrue()) in
            let notArr = Array.create (Seq.length outgoing) (ctx.MkTrue()) in
            for i in 0 .. (Seq.length outgoing - 1) do
                let e = Seq.item i outgoing in
                let eVar = Map.find (e.Source, e.Target) eMap in
                Array.set arr i (Array2D.get eArray eVar index);
                Array.set notArr i (ctx.MkNot (Array2D.get eArray eVar index));
            let exp = ctx.MkAtMost(arr, 1u) in
            let notexp =
                if Seq.length outgoing > 0 then
                    ctx.MkAtMost(notArr, ((uint32) (Seq.length outgoing) - 1u))
                else 
                    ctx.MkTrue() in
            let combArr = Array.create 2 notexp in
            Array.set combArr 1 exp;
            condSet <- Set.add (ctx.MkImplies ((Array2D.get vArray j index), ctx.MkAnd(combArr))) condSet;
        

        // relationship between nodes with the same topological location
        // identify using CgState.Node field which gives the topo.node
        // basically put all of these variables in one set and use a mutual
        //implication statement?
        //use Start and End in CGraph, map them to topological node and use
        // them for first connectivity constraint

        //Console.Write("loopfree");
        let mutable topoNodeToVertexSet = Map.empty in
        for i in 0 .. (Seq.length vertices - 1) do
            let vertex = Seq.item i vertices in
            let vertexExp = Array2D.get vArray (Map.find (Seq.item i vertices) vMap) index in
            let topoNode = vertex.Node in
            if (Map.containsKey topoNode topoNodeToVertexSet) then
                let newSet = Set.add vertexExp (Map.find topoNode topoNodeToVertexSet) in
                topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;
            else
                let newSet = Set.add vertexExp Set.empty in
                topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;

        //iterate through this map, creating statements per set for topological Node 
        //Console.Write("actually adding loopfree condiitons");
        let prepCondition key value = 
            let exp = ctx.MkAtMost((Set.toArray value), 1u) in
            condSet <- Set.add exp condSet;
        in
        Map.iter prepCondition topoNodeToVertexSet;

        Array.set sameTopoCond index condSet;
    sameTopoCond
    

// generates test for preference coverage for this given predicate
let genPrefTest (input: CGraph.T) (coverage : int) (pred : Route.Predicate) : TestCases*float =
    let ctx = new Context() in
    let mutable tests = Set.empty
    let vertices = input.Graph.Vertices
    let edges = input.Graph.Edges

    //Console.Write("Product graph edges");
    //for e in edges do
    //    Console.Write("(" + (string e.Source) + "," + (string e.Target) + ")");
    //    Console.Write("\n");

    // find map between topological node and all the cgraph nodes that have the same topo node in order of preference
    //Console.Write("loopfree");
    let vertexToCGraphNodes : Dictionary<String, seq<CGraph.CgState>> =
        match (Consistency.findOrderingConservative 1 input) with
        | Ok ord -> ord
        | _ -> Dictionary<String, seq<CgState>>();

    // generate a random set of vertices to cover
    let getVertices curSeq k v = Seq.append curSeq (Seq.tail v)
    let allVertices = Dictionary.fold getVertices Seq.empty vertexToCGraphNodes
    let origSize = Seq.length allVertices
    let mutable verticesToCover = Set.empty
    if (coverage = 100) then
        verticesToCover <- Set.ofSeq allVertices
    else 
        let R = System.Random()
        while (float (Set.count verticesToCover) < (float coverage) / 100.0 * (float origSize)) do 
            let newindex = R.Next(1, origSize)
            if (not (Set.contains (Seq.item newindex allVertices) verticesToCover)) then
                verticesToCover <- Set.add (Seq.item newindex allVertices) verticesToCover;
            else ();    

    //cretae vertex map
    //Console.Write("creating vertex map");

    for kv in vertexToCGraphNodes do
        let topoNode = kv.Key
        let vertexSet = kv.Value

        if (Seq.length vertexSet > 1) then 
            let vArray = Array2D.zeroCreate (Seq.length vertices) (Seq.length vertexSet)
            let eArray = Array2D.zeroCreate (Seq.length edges) (Seq.length vertexSet) 
            let vIntArray = Array2D.zeroCreate (Seq.length vertices) (Seq.length vertexSet)

            for j in 0 .. (Seq.length vertexSet - 1) do
                for i in 0 .. (Seq.length vertices - 1) do
                    Array2D.set vArray i j (ctx.MkBoolConst ("v" + (string i) + (string j)));
                    Array2D.set vIntArray i j (ctx.MkIntConst ("vI" + (string i) + (string j)));
                for i in 0 .. (Seq.length edges - 1) do
                    Array2D.set eArray i j (ctx.MkBoolConst ("e" + (string i) + (string j)))
                

            for i in 0 .. (Seq.length vertexSet - 2) do
                let firstVertex = Seq.item i vertexSet in // TODO: this needs to be in preference order
                let secondVertex = Seq.item (i + 1) vertexSet

                if (Set.contains secondVertex verticesToCover) then

                    //Console.Write("here looking at vertex neighbors: " + (string firstVertex) + " " + (string secondVertex))
                    
                    // create K equivalent SAT problems
                    // use new variables that if true imply that that particular problem is true?
                    // then you want first and second to be true but rest to be false
                    // use an array of condSets and set one to true and other to false
                    let problemSet = getPrefIndividualProblems input ctx vertexSet vArray eArray vIntArray

                    // make the solver and iterate through it
                    //Console.Write("make solver and iterate");
                    let s = ctx.MkSolver()
                    let mutable condSet = Set.empty;

                    for j in 0 .. (Seq.length problemSet - 1) do
                        let curProblem = Seq.item j problemSet
                        if (j = i || j = (i + 1)) then
                            //Console.Write("writing my own nodes");
                            condSet <- Set.add (ctx.MkAnd(Set.toArray curProblem)) condSet;
                        else
                            condSet <- Set.add (ctx.MkNot(ctx.MkAnd(Set.toArray curProblem))) condSet;
                        

                    s.Assert(ctx.MkAnd (Set.toArray condSet));
                    //File.AppendAllText("solutions.txt", "New Set for prefix \n")
                    if (s.Check() = Status.SATISFIABLE) then
                        let mutable curPath = Set.empty in
                        let mutable expectedPath = Set.empty in
                        verticesToCover <- Set.remove secondVertex verticesToCover
                        //let mutable isOutPath = false in
                        //File.AppendAllText("solutions.txt", "New Solution\n")
                        for j in 0 .. (Seq.length edges - 1) do
                            if (s.Model.ConstInterp(Array2D.get eArray j i).IsTrue || s.Model.ConstInterp(Array2D.get eArray j (i + 1)).IsTrue) then
                                //add current edge to the current Path
                                let edge = Seq.item j edges in
                                //if Topology.isTopoNode edge.Source.Node then
                                curPath <- Set.add (edge.Source, edge.Target) curPath;
                                //else 
                                //    ();

                                if (s.Model.ConstInterp(Array2D.get eArray j i).IsTrue) then
                                    expectedPath <- Set.add (edge.Source, edge.Target) expectedPath;

                                //if (Topology.isUnknown edge.Source.Node || Topology.isUnknown edge.Target.Node)  then
                                //    isOutPath <- true

                            // File.AppendAllText("solutions.txt", (string) (Seq.item j edges) + "\n");
                                //bgp peer up? for cbgp file
                            else
                                ();
                        if (Set.count curPath > 2) then 
                            tests <- Set.add (curPath, expectedPath) tests;
                    //else Console.Write("cannot find, bailing");
                    //File.AppendAllText("solutions.txt", "\n")
                    //Console.Write("done");
    if (origSize = 0) then
        (tests, -1.0)
    else
        (tests, (1.0 - (float (Set.count verticesToCover))/ (float origSize)))

    //done  

let runTest =
    ()