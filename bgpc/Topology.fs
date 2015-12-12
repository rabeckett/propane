module Topology
open QuickGraph
open QuickGraph.Algorithms

type NodeType = 
    | Start
    | End
    | Outside
    | Inside 
    | InsideOriginates

type State = 
    {Loc: string; 
     Typ: NodeType}

type T = BidirectionalGraph<State,TaggedEdge<State,unit>>

let setOriginators (topo: T) (orig: Set<string>) : T =
    let mutable conv = Map.empty
    let newTopo = BidirectionalGraph()
    for v in topo.Vertices do 
        if orig.Contains v.Loc then
            match v.Typ with 
            | Inside | InsideOriginates ->
                let nv = {Loc=v.Loc; Typ=InsideOriginates} 
                newTopo.AddVertex nv |> ignore
                conv <- Map.add v nv conv
            | _ ->
                failwith ("[Topology Error]: cannot set location: " + v.Loc + " to be internal")
        else 
            newTopo.AddVertex v |> ignore
            conv <- Map.add v v conv

    for e in topo.Edges do 
        let v = Map.find e.Source conv
        let u = Map.find e.Target conv
        newTopo.AddEdge (TaggedEdge<State,unit>(v,u,())) |> ignore
    newTopo

let alphabet (topo: T) : Set<State> * Set<State> = 
    let mutable ain = Set.empty 
    let mutable aout = Set.empty 
    for v in topo.Vertices do
        match v.Typ with 
        | Inside -> ain <- Set.add v ain
        | InsideOriginates -> ain <- Set.add v ain
        | Outside -> aout <- Set.add v aout
        | Start | End -> failwith "unreachable"
    (ain, aout)

let isTopoNode (t: State) = 
    match t.Typ with 
    | Start | End -> false
    | Outside | Inside | InsideOriginates -> true

let isInside (t: State) = 
    match t.Typ with 
    | Inside | InsideOriginates ->  true 
    | Outside | Start | End -> false

let canOriginateTraffic (t: State) = 
    match t.Typ with 
    | InsideOriginates -> true 
    | Outside -> true
    | Inside -> false
    | Start | End -> false

(* TODO *)
let isWellFormed (t: State) = 
    false

let rec addVertices (topo: T) (vs: State list) = 
    match vs with 
    | [] -> ()
    | v::vs -> 
        topo.AddVertex v |> ignore
        addVertices topo vs

let rec addEdgesUndirected (topo: T) (es: (State * State) list) = 
    match es with 
    | [] -> () 
    | (x,y)::es -> 
        topo.AddEdge (TaggedEdge(x,y,())) |> ignore
        topo.AddEdge (TaggedEdge(y,x,())) |> ignore
        addEdgesUndirected topo es

let rec addEdgesDirected (topo: T) (es: (State * State) list) = 
    match es with 
    | [] -> () 
    | (x,y)::es -> 
        topo.AddEdge (TaggedEdge(x,y,())) |> ignore
        addEdgesDirected topo es


module Failure =
    type FailType =
        | NodeFailure of State
        | LinkFailure of TaggedEdge<State,unit>
        
        override this.ToString() = 
            match this with 
            | NodeFailure n -> "Node(" + n.Loc + ")"
            | LinkFailure e -> "Link(" + e.Source.Loc + "," + e.Target.Loc + ")"
  
    let allFailures n (topo: T) : seq<FailType list> =
        let fvs = topo.Vertices |> Seq.filter isInside |> Seq.map NodeFailure
        let fes =
            topo.Edges
            |> Seq.filter (fun e -> isInside e.Source || isInside e.Target) 
            |> Seq.map LinkFailure 
        Seq.append fes fvs 
        |> Seq.toList
        |> Common.List.combinations n
