open Common.Debug
open Common.Error


[<EntryPoint>]
let main argv =
    let opts = Args.parse argv
    let settings = Args.getSettings ()
    logInfo0 (sprintf "Got settings: %A" settings)
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
        | None -> error ("No policy file specified")
        | Some p ->
            let ast = Input.readFromFile p
            
            let pairs = Ast.makePolicyPairs ast reb
            let (prefixes,res) = pairs.Head
            printfn "%A" pairs

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
                        error (sprintf "Unused preferences %A" m)
                    | IR.NoPathForRouters rs ->
                        error (sprintf "Unable to find a path for routers: %A" rs)
                    | IR.InconsistentPrefs(x,y) ->
                        let xs = x.ToString()
                        let ys = y.ToString() 
                        error (sprintf "Unable to implement in BGP. Can not choose between:\n%s\n%s" xs ys)
            | Args.Template -> ()

    0