open Common
open Common.Debug
open Common.Error


[<EntryPoint>]
let main argv =
    (* Parse command line settings *)
    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    logInfo0 (sprintf "Got settings: %A" settings)

    (* Run unit tests if in test mode *)
    if settings.Test then
        Test.run ()
        exit 0

    (* Set debugging output file *)
    let fileName = Option.getOrDefault "output" settings.OutFile
    let fullName = settings.DebugDir + (string System.IO.Path.DirectorySeparatorChar) + fileName

    let topo = Examples.topoDatacenterSmall()
    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p
        let pairs = Ast.makePolicyPairs ast topo
        let (prefixes, reb, res) = pairs.Head

        match settings.Format with 
        | Args.IR ->
            match IR.compileToIR reb res fullName with 
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
                    unimplementable (sprintf "Unable to find a path for routers: %A" rs)
                | IR.InconsistentPrefs(x,y) ->
                    let xs = x.ToString()
                    let ys = y.ToString() 
                    unimplementable (sprintf "Can not choose preference between:\n%s\n%s" xs ys)
                | IR.UncontrollableEnter x -> 
                    let xs = x.ToString()
                    unimplementable (sprintf "Can not control inbound traffic for peer: %s" xs)
        | Args.Template -> ()

    0