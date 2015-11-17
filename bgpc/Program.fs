open Extension.Error


let topo = Examples.topoDatacenterSmall()
let reb = Regex.REBuilder(topo)

let chooseFirst (ast: Ast.T) = 
    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex reb r) res
    res

[<EntryPoint>]
let main argv = 
    let options = Args.parse argv

    if options.Test then 
        Test.init ()
        Test.run ()
    else
        let ast = Input.readFromFile options.PolFile
        let res = chooseFirst ast
        let cg = CGraph.buildFromRegex topo reb res
        Minimize.pruneHeuristic cg
    
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