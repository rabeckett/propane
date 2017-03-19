module TestGenerator

open CGraph
open Microsoft.Z3
open Topology
open System

type T = 
    {
        Model : Solver
    }

let genTest (input: CGraph.T) : unit =
    let ctx = new Context() in
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in
    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in

    //cretae vertex map
    //Console.Write("creating vertex map");
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBoolConst ("v" + (string i)));
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
    let target = Map.find input.End vMap in
    condSet <- Set.add (Array.get vArray target) condSet ;
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
    System.IO.File.WriteAllText("solutions.txt", "New Set for prefix \n")
    while (s.Check() = Status.SATISFIABLE) do
      //Console.Write("iterating once \n");
      let mutable solnSet = Set.empty in
      System.IO.File.AppendAllText("solutions.txt", "New Solution\n")
      for i in 0 .. (Seq.length edges - 1) do
        if (s.Model.ConstInterp(eArray.[i]).IsTrue) then
            solnSet <- Set.add eArray.[i] solnSet;
            System.IO.File.AppendAllText("solutions.txt", (string) (Seq.item i edges) + "\n");
            //let edge = Seq.item i edges in
            //System.IO.File.AppendAllText("solutions.txt", (string) (edge.Source.Node) + "->" + (string) (edge.Target.Node) + ")\n")
        else
            ();
      System.IO.File.AppendAllText("solutions.txt", "\n")
      let negSoln = ctx.MkNot(ctx.MkAnd(Set.toArray solnSet)) in
      condSet <- Set.add negSoln condSet;
      s.Assert(Set.toArray condSet);
    Console.Write("done");
    //done  

let runTest =
    ()