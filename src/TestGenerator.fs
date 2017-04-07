module TestGenerator

open CGraph
open Microsoft.Z3
open Topology
open System
open System.Net
open System.IO

// set of edges where each edge is a pair of vertices
type Path = Set<CgState*CgState> 
type TestCases = Set<Path>

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

let getCBGPpeerSessions (vertexToPeers : Map<CgState, Set<CgState>>) (routerNameToIp : Map<string, string>) file : unit =
    let printSingleRouter router neighbors =
        if not (Seq.isEmpty neighbors) then
            let routerIp = Map.find router.Node.Loc routerNameToIp
            let routerStr = "\nbgp router " + routerIp
            File.AppendAllText(file, routerStr);
            for n in neighbors do
                let peerNum = n.Node.Loc
                let peerIp = Map.find peerNum routerNameToIp
                let peerStr = "\n    add peer " + peerNum + " " + peerIp
                let nextHopStr = "\n    peer " + peerIp + " next-hop-self"
                let upStr = "\n    peer " + peerIp + " up"
                File.AppendAllText(file, peerStr + nextHopStr + upStr);
            File.AppendAllText(file, "\n    exit");
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
        if (not (Topology.isUnknown vertex)) then 
            let toWrite = "net add node " + ipOfInt ((uint32) (i + 1)) in
            File.AppendAllText(file, toWrite + "\n");

    //create links for the edges
    for e in Topology.edges input do
        let (src, target) = e
        let srcIdx = Map.find src vMap
        let targetIdx = Map.find target vMap
        if (srcIdx < targetIdx) then 
            let toWrite = ipOfInt ((uint32) srcIdx) + " " + ipOfInt ((uint32) targetIdx) in
            File.AppendAllText(file, "net add link " + toWrite + "\n");
        // should i be adding the bGP router/igp stuff right here -rulelessly   

// generates the link ocverage tests for the given predicate
let genLinkTest (input: CGraph.T) (pred : Route.Predicate) : TestCases =
    let ctx = new Context() in
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in
    let filterf (n1,n2) = Topology.isTopoNode n1 && Topology.isTopoNode n2 
                            && (not (Topology.isUnknown n1)) && (not (Topology.isUnknown n2))
    let edgesToCover = Seq.filter filterf (Topology.edges input.Topo) 
    

    Console.Write("edges");
    for (src, dst) in edgesToCover do
        Console.Write("(" + src.Loc + "," + dst.Loc + ")");
        Console.Write("\n");

    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    let vIntArray = Array.zeroCreate (Seq.length vertices) in
    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in
    let mutable edgesSoFar : Set<Topology.Node * Topology.Node> = Set.empty

    //cretae vertex map
    //Console.Write("creating vertex map");
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBoolConst ("v" + (string i)));
        Array.set vIntArray i (ctx.MkIntConst ("vI" + (string i)));
        vMap <- Map.add (Seq.item i vertices)  i vMap;
    let eArray = Array.zeroCreate (Seq.length edges) in

    // create edge map
    //Console.Write("creating edge map");
    for i in 0 .. (Seq.length edges - 1) do
        Array.set eArray i (ctx.MkBoolConst ("e" + (string i)));
        let edge = Seq.item i edges in
        eMap <- Map.add (edge.Source, edge.Target) i eMap;

    let mutable condSet = Set.empty in
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
        //let srcPlusOne = ctx.MkAdd(vIntArray.[a], ctx.MkInt(1)) in
        //condSet <- Set.add (ctx.MkEq(vIntArray.[b], srcPlusOne)) condSet;
        condSet <- Set.add (ctx.MkGt(vIntArray.[b], vIntArray.[a])) condSet;

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
    //Console.Write("make solver and iterate");
    let s = ctx.MkSolver()  
    s.Assert(Set.toArray condSet);
    File.AppendAllText("solutions.txt", "New Set for prefix \n")
    let mutable tests = Set.empty in
    while (s.Check() = Status.SATISFIABLE && (Set.count edgesSoFar < Seq.length edgesToCover)) do
      Console.Write("iterating once \n");
      Console.Write("edges so far " + (string) (Set.count edgesSoFar) + "total edges " + (string) (Seq.length edgesToCover) + "\n");
      Console.Write("edges covered so far");
      for (src, dst) in edgesSoFar do
        Console.Write("(" + src.Loc + "," + dst.Loc + ")");
        Console.Write("\n");

      let mutable solnSet = Set.empty in
      let mutable curPath = Set.empty in
      File.AppendAllText("solutions.txt", "New Solution\n")
      for i in 0 .. (Seq.length edges - 1) do
        if (s.Model.ConstInterp(eArray.[i]).IsTrue) then
            solnSet <- Set.add eArray.[i] solnSet;

            //add current edge to the current Path
            let edge = Seq.item i edges in
            if (Topology.isTopoNode edge.Source.Node && Topology.isTopoNode edge.Target.Node) then
                edgesSoFar <- Set.add (edge.Source.Node, edge.Target.Node) edgesSoFar;
                edgesSoFar <- Set.add (edge.Target.Node, edge.Source.Node) edgesSoFar;
            if Topology.isTopoNode edge.Source.Node then
                curPath <- Set.add (edge.Source, edge.Target) curPath;
            else 
                ();

            let a = s.Model.Evaluate(vIntArray.[Map.find edge.Source vMap]) in
            let b = s.Model.Evaluate(vIntArray.[Map.find edge.Target vMap]) in
            File.AppendAllText("solutions.txt", (string) (Seq.item i edges) + " " + (string a) + " " + (string b) + "\n");
            
            //bgp peer up? for cbgp file
        else
            ();
      tests <- Set.add curPath tests;
      File.AppendAllText("solutions.txt", "\n")
      let negSoln = ctx.MkNot(ctx.MkAnd(Set.toArray solnSet)) in
      condSet <- Set.add negSoln condSet;
      s.Assert(Set.toArray condSet);
    Console.Write("done");
    tests


// generates the individual pref tests for all nodes that share a given topological node
let getPrefIndividualProblems (input: CGraph.T) (ctx : Context) k vArray eArray: Set<Set<BoolExpr>> =
    let mutable sameTopoCond = Set.empty
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in

    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in

    //cretae vertex map
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
        let target = Map.find input.End vMap in
        condSet <- Set.add (Array2D.get vArray target index) condSet ;
        condSet <- Set.add (Array2D.get vArray index index) condSet;
    
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
            let combArr = Array.create 2 exp in
            let notexp =
                if Seq.length outgoing > 0 then
                    ctx.MkAtMost(notArr, ((uint32) (Seq.length outgoing) - 1u))
                else 
                    ctx.MkTrue() in
            Array.set combArr 1 notexp;
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

        sameTopoCond <- Set.add condSet sameTopoCond;
    sameTopoCond
    

// generates test for preference coverage for this given predicate
let genPrefTest (input: CGraph.T) (pred : Route.Predicate) : TestCases =
    let ctx = new Context() in
    let mutable tests = Set.empty
    let vertices = input.Graph.Vertices
    let edges = input.Graph.Edges

    // find map between topological node and all the cgraph nodes that have the same topo node
    //Console.Write("loopfree");
    let mutable topoNodeToVertexSet = Map.empty in
    for i in 0 .. (Seq.length vertices - 1) do
        let vertex = Seq.item i vertices in
        //let vertexExp = vArray.[Map.find (Seq.item i vertices) vMap] in
        let topoNode = vertex.Node in
        if (Map.containsKey topoNode topoNodeToVertexSet) then
            let newSet = Set.add vertex (Map.find topoNode topoNodeToVertexSet) in
            topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;
        else
            let newSet = Set.add vertex Set.empty in
            topoNodeToVertexSet <- Map.add topoNode newSet topoNodeToVertexSet;

    //cretae vertex map
    //Console.Write("creating vertex map");

    for kv in topoNodeToVertexSet do
        let topoNode = kv.Key
        let vertexSet = kv.Value

        let vArray = Array2D.zeroCreate (Seq.length vertices) (Set.count vertexSet) in
        let eArray = Array2D.zeroCreate (Seq.length edges) (Set.count vertexSet) 

        for j in 0 .. (Set.count vertexSet - 1) do
            for i in 0 .. (Seq.length vertices - 1) do
                Array2D.set vArray i j (ctx.MkBoolConst ("v" + (string i)));
            for i in 0 .. (Seq.length edges - 1) do
                Array2D.set eArray i j (ctx.MkBoolConst ("e" + (string i)));
            

        for i in 0 .. (Seq.length vertexSet - 2) do
            let firstVertex = Seq.item i vertexSet in // TODO: this needs to be in preference order
            let secondVertex = Seq.item (i + 1) vertexSet
            
            // create K equivalent SAT problems
            // use new variables that if true imply that that particular problem is true?
            // then you want first and second to be true but rest to be false
            // use an array of condSets and set one to true and other to false
            let problemSet = getPrefIndividualProblems input ctx (Seq.length vertexSet) vArray eArray

            // make the solver and iterate through it
            //Console.Write("make solver and iterate");
            let s = ctx.MkSolver()
            let mutable condSet = Set.empty;

            for j in 0 .. (Seq.length problemSet - 1) do
                let curProblem = Seq.item j problemSet
                if (j = i || j = (i + 1)) then 
                    condSet <- Set.add (ctx.MkAnd(Set.toArray curProblem)) condSet;
                else
                    condSet <- Set.add (ctx.MkNot(ctx.MkAnd(Set.toArray curProblem))) condSet;

            s.Assert(Set.toArray condSet);
            File.AppendAllText("solutions.txt", "New Set for prefix \n")
            if (s.Check() = Status.SATISFIABLE) then
                //Console.Write("iterating once \n");
                let mutable curPath = Set.empty in
                File.AppendAllText("solutions.txt", "New Solution\n")
                for j in 0 .. (Seq.length edges - 1) do
                    if (s.Model.ConstInterp(Array2D.get eArray j i).IsTrue || s.Model.ConstInterp(Array2D.get eArray j (i + 1)).IsTrue) then
                        //add current edge to the current Path
                        let edge = Seq.item j edges in
                        if Topology.isTopoNode edge.Source.Node then
                            curPath <- Set.add (edge.Source, edge.Target) curPath;
                        else 
                            ();

                        File.AppendAllText("solutions.txt", (string) (Seq.item i edges) + "\n");
                        //bgp peer up? for cbgp file
                    else
                        ();
                tests <- Set.add curPath tests;
            else ();
            File.AppendAllText("solutions.txt", "\n")
            Console.Write("done");
    tests

    
    //let newPredToTestCases = Map.add pred tests cbgpTests.predToTestCases 
    //{
    //    routerNameToIpMap = cbgpTests.routerNameToIpMap
    //    predToTestCases = newPredToTestCases
    //} 

    //done  

let runTest =
    ()