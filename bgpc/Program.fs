open Extension.Error


let topo = Topology.Example2.topo ()
let RE = Regex.REBuilder(topo)

[<EntryPoint>]
let main argv = 
    let options = Args.parse argv
    let ast = Input.readFromFile options.PolFile

    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex RE r) res
    let autos = List.map (fun r -> RE.MakeDFA (RE.Rev r)) res |> Array.ofList
    let cg = CGraph.build topo autos

    Minimize.removeEdgesForDominatedNodes cg
    Minimize.removeNodesNotOnAnySimplePathToEnd cg
    
    match options.Format with 
    | Args.IR ->
        match Config.compile topo cg with 
        | Ok(config) ->
            match options.OutFile with 
            | None -> () 
            | Some out ->
                System.IO.File.WriteAllText(out, Config.format config)
        | Err((x,y)) -> ()
    | Args.Template -> ()
    | Args.Graph ->
        match options.OutFile with
        | None -> ()
        | Some out ->
            System.IO.File.WriteAllText(out, CGraph.toDot cg)

    0