open Common.Error


let chooseFirst (ast: Ast.T) reb = 
    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex reb r) res
    res

[<EntryPoint>]
let main argv =
    let opts = Options.parse argv

    if opts.Test then 
        Test.run ()
    else
        let topo = Examples.topoDatacenterSmall()
        let reb = Regex.REBuilder(topo)

        match opts.PolFile with 
        | None -> 
            printfn "No policy file specified"
            exit 0
        | Some p ->
            let ast = Input.readFromFile p
            let res = chooseFirst ast reb
            let cg = CGraph.buildFromRegex topo reb res
            match opts.Format with 
            | Options.IR ->
                match IR.compileToIR topo reb res opts.OutFile with 
                | Ok(config) -> ()
                | Err(_) -> ()
            | Options.Template -> ()
            | Options.Graph ->
                match opts.OutFile with
                | None -> ()
                | Some out ->
                    System.IO.File.WriteAllText(out, CGraph.toDot cg)

    0