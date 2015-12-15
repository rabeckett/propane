open Common.Error

let chooseFirst (ast: Ast.T) reb = 
    let scope1 = ast.Head 
    let (_, res) = scope1.PConstraints.Head
    let res = List.map (fun r -> Ast.buildRegex reb r) res
    res

[<EntryPoint>]
let main argv =
    let opts = Args.parse argv
    let settings = Args.getSettings ()
    if settings.Test then 
        Test.run () 
    else
        let outName = 
            match settings.OutFile with 
            | None -> settings.DebugDir + string System.IO.Path.DirectorySeparatorChar + "output" 
            | Some n -> settings.DebugDir + string System.IO.Path.DirectorySeparatorChar + n
        let topo = Examples.topoDatacenterSmall()
        let reb = Regex.REBuilder(topo)
        match settings.PolFile with 
        | None -> 
            printfn "No policy file specified"
            exit 0
        | Some p ->
            let ast = Input.readFromFile p
            let res = chooseFirst ast reb
            let cg = CGraph.buildFromRegex topo reb res
            match settings.Format with 
            | Args.IR ->
                match IR.compileToIR topo reb res outName with 
                | Ok(config) -> 
                    match settings.OutFile with
                    | None -> ()
                    | Some out ->
                        System.IO.File.WriteAllText(out + ".ir", IR.format config)
                | Err(x) -> 
                    match x with
                    | IR.UnusedPreferences m -> 
                        printfn "Error: Unused preferences %A" m
                        exit 0
                    | IR.NoPathForRouters rs -> 
                        printfn "Error: Unable to find a path for routers: %A" rs
                        exit 0
                    | IR.InconsistentPrefs(x,y) -> 
                        printfn "Error: Unable to implement in BGP. Can not choose between:"
                        printfn "%s" (x.ToString()) 
                        printfn "%s" (y.ToString())
                        exit 0
            | Args.Template -> ()

    0