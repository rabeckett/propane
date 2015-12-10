open Extension.Error


let chooseFirst (ast: Ast.T) reb = 
    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex reb r) res
    res

[<EntryPoint>]
let main argv =
    let options = Args.parse argv

    if options.Test then 
        Test.run ()
    else
        let topo = Examples.topoDatacenterSmall()
        let reb = Regex.REBuilder(topo)

        match options.PolFile with 
        | None -> 
            printfn "No policy file specified"
            exit 0
        | Some p ->
            let ast = Input.readFromFile p
            let res = chooseFirst ast reb
            let cg = CGraph.buildFromRegex topo reb res
            CGraph.Minimize.pruneHeuristic cg
            match options.Format with 
            | Args.IR ->
                match IR.compileToIR topo reb res options.OutFile with 
                | Ok(config) -> ()
                | Err(_) -> ()
            | Args.Template -> ()
            | Args.Graph ->
                match options.OutFile with
                | None -> ()
                | Some out ->
                    System.IO.File.WriteAllText(out, CGraph.toDot cg)

    0