module TestGenerator

open CGraph
open Microsoft.Z3
open Topology

type T = 
    {
        Model : Solver
    }

let genTest (input: CGraph.T) (ctx : Context) : Solver =
    let vertices = input.Graph.Vertices in
    let edges = input.Graph.Edges in
    // array of boolExpr for vertices and edges respectively
    let vArray = Array.zeroCreate (Seq.length vertices) in
    let mutable vMap = Map.empty in
    let mutable eMap = Map.empty in
    for i in 0 .. (Seq.length vertices - 1) do
        Array.set vArray i (ctx.MkBoolConst ("v" + (string i)));
        vMap <- Map.add (Seq.item i vertices)  i vMap;
    let eArray = Array.zeroCreate (Seq.length edges) in
    for i in 0 .. (Seq.length edges - 1) do
        // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
        Array.set eArray i (ctx.MkBoolConst ("e" + (string i)));
        eMap <- Map.add (Seq.item i edges) i eMap;
    let mutable condSet = Set.empty in
    let src = Map.find input.Start vMap in
    condSet <- Set.add (Array.get vArray src) condSet ;
    let target = Map.find input.End vMap in
    condSet <- Set.add (Array.get vArray target) condSet ;
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
    for i in 0 .. (Seq.length vertices - 1) do
        // find vertices at the start and end of an edge for implication between edges and vertices for connectivity
        let incomingEdges = Seq.item i edges in
        let a = Map.find edge.Source vMap in
        let b = Map.find edge.Target vMap in
        let arr = Array.create 2 vArray.[a] in
        Array.set arr 1 vArray.[b];
        let ends = ctx.MkAnd arr in
        let exp = ctx.MkImplies (eArray.[i], ends) in
        condSet <- Set.add exp condSet;
    let finalExp = ctx.MkAnd (Set.toArray condSet) in

    // relationship between nodes with the same topological location
    // identify using CgState.Node field which gives the topo.node
    // basically put all of these variables in one set and use a mutual
    //implication statement?
    //use Start and End in CGraph, map them to topological node and use
    // them for first connectivity constraint
    ctx.MkSolver ()

let runTest =
    ()