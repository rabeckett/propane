open Extension.Error



let topo = Topology.Example2.topo ()
let RE = Regex.REBuilder(topo)

[<EntryPoint>]
let main argv = 
    let options = Args.parse argv
    let ast = Input.readFromFile options.PolFile

    printfn "AST: %A" ast

    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex RE r) res
    let autos = List.map (fun r -> RE.MakeDFA (RE.Rev r)) res |> Array.ofList
    let cg = CGraph.build topo autos

    Minimize.removeEdgesForDominatedNodes cg
    Minimize.removeNodesNotOnAnySimplePathToEnd cg
    
    match options.Format with 
    | Args.IR -> ()
    | Args.Template -> ()
    | Args.Graph ->
        match options.OutFile with
        | None -> ()
        | Some out ->
            System.IO.File.WriteAllText(out, CGraph.toDot cg)

    (*
    match Config.compile topo cg with
    | Ok(config) -> Config.print config
    | Err(Consistency.PrefViolation (x,y)) ->
        printfn "Pref Consistency Violation" 
        printfn "Node1: %A" x 
        printfn "Node2: %A" y
    | Err(Consistency.TopoViolation (x,y)) ->
        printfn "Topo Consistency Violation"
        printfn "Node1: %A" x 
        printfn "Node2: %A" y *)

    0