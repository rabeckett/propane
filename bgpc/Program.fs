
open QuickGraph
open ConstraintGraph
open Policy

open System.Diagnostics


module Example1 = 
    
    let topo () = 
        let g = BidirectionalGraph<TopoState,TaggedEdge<TopoState,unit>>()

        let vA = {loc="A"; typ=InsideHost}
        let vX = {loc="X"; typ=Inside}
        let vM = {loc="M"; typ=Inside}
        let vN = {loc="N"; typ=Inside}
        let vY = {loc="Y"; typ=Inside}
        let vZ = {loc="Z"; typ=Inside}
        let vB = {loc="B"; typ=InsideHost}

        g.AddVertex vA |> ignore 
        g.AddVertex vX |> ignore 
        g.AddVertex vM |> ignore 
        g.AddVertex vN |> ignore 
        g.AddVertex vY |> ignore 
        g.AddVertex vZ |> ignore 
        g.AddVertex vB |> ignore 

        g.AddEdge (TaggedEdge(vA, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vA, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vZ, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vZ, vB, ())) |> ignore
        
        g


module Example2 = 
    
    let topo () = 
        let g = BidirectionalGraph<TopoState,TaggedEdge<TopoState,unit>>()

        let vA = {loc="A"; typ=InsideHost}
        let vB = {loc="B"; typ=InsideHost}
        let vC = {loc="C"; typ=InsideHost}
        let vD = {loc="D"; typ=InsideHost}
        let vX = {loc="X"; typ=Inside}
        let vY = {loc="Y"; typ=Inside}
        let vM = {loc="M"; typ=Inside}
        let vN = {loc="N"; typ=Inside}

        g.AddVertex vA |> ignore 
        g.AddVertex vB |> ignore 
        g.AddVertex vC |> ignore 
        g.AddVertex vD |> ignore 
        g.AddVertex vX |> ignore 
        g.AddVertex vY |> ignore 
        g.AddVertex vM |> ignore 
        g.AddVertex vN |> ignore 

        g.AddEdge (TaggedEdge(vA, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vA, ())) |> ignore
        g.AddEdge (TaggedEdge(vB, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vB, ())) |> ignore
        g.AddEdge (TaggedEdge(vC, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vC, ())) |> ignore
        g.AddEdge (TaggedEdge(vD, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vD, ())) |> ignore

        g.AddEdge (TaggedEdge(vX, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vX, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vX, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vM, ())) |> ignore
        g.AddEdge (TaggedEdge(vM, vY, ())) |> ignore
        g.AddEdge (TaggedEdge(vY, vN, ())) |> ignore
        g.AddEdge (TaggedEdge(vN, vY, ())) |> ignore

        g


let RE = RegularExpression(Set.ofList ["A"; "B"; "C"; "D"; "X"; "Y"; "M"; "N"], Set.empty)


(* Format Graphviz output *)
let OnFormatEdge(e: Graphviz.FormatEdgeEventArgs<CgState, TaggedEdge<CgState,unit>>) = 
    ()
   
let OnFormatVertex(v: Graphviz.FormatVertexEventArgs<CgState>) = 
    let states = Array.map string v.Vertex.states |> String.concat ", "
    let location = v.Vertex.topo.loc.ToString()
    match v.Vertex.accept with 
    | None -> 
        v.VertexFormatter.Label <- "(" + states + ", " + location + ")"
    | Some i ->
        v.VertexFormatter.Label <- "(" + states + ", " + location + ")\npref=" + (string i)
        v.VertexFormatter.Shape <- Graphviz.Dot.GraphvizVertexShape.DoubleCircle
        v.VertexFormatter.Style <- Graphviz.Dot.GraphvizVertexStyle.Filled
        v.VertexFormatter.FillColor <- Graphviz.Dot.GraphvizColor.LightYellow


[<EntryPoint>]
let main argv = 
    (* let r = RE.concat (RE.loc "A") (RE.concat (RE.loc "X") (RE.concat (RE.loc "N") (RE.concat (RE.loc "Y") (RE.loc "B")) )) *)
    (* let r = RE.star RE.inside *)

    let x = RE.star RE.inside
    let r1 = RE.concat (RE.concat x (RE.loc "M")) x
    let r2 = RE.concat (RE.concat x (RE.loc "N")) x

    let dfa1 = RE.makeDFA 1 (RE.rev r1)
    let dfa2 = RE.makeDFA 2 (RE.rev r2)
        
    let cg = ConstraintGraph.build (Example2.topo()) [|dfa1; dfa2|] 

    ConstraintGraph.removeDeadStates cg
    
    (* Generate DOT graph output *)
    let graphviz = Graphviz.GraphvizAlgorithm<CgState, TaggedEdge<CgState,unit>>(cg.graph)
    graphviz.FormatEdge.Add(OnFormatEdge)
    graphviz.FormatVertex.Add(OnFormatVertex)
    let output = graphviz.Generate()
   
    printfn "%s" output

    (* ConstraintGraph.compile cg *)

    (*
    System.IO.File.WriteAllText("graph.dot", output)

    (* Render a PNG using the graphviz executable *)
    let path = System.Environment.ExpandEnvironmentVariables(@"%PROGRAMFILES%\Graphviz2.38\bin\dot.exe");
    let info = new ProcessStartInfo(path)
    let p = new System.Diagnostics.Process()
    p.StartInfo <- info
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.Arguments <- "-Tpng graph.dot"
    p.Start () |> ignore
    let output = p.StandardOutput.ReadToEnd ()
    p.WaitForExit () |> ignore
    System.IO.File.WriteAllText("graph.png", output)
    *)

    0


