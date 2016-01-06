open Common
open Common.Debug
open Common.Error



let compileForSinglePrefix (settings: Args.T) fullName (prefix, reb, res) =
    try 
        match IR.compileToIR prefix reb res fullName with 
        | Ok(config) -> config
            (* match settings.OutFile with
            | None -> ()
            | Some out ->
                System.IO.File.WriteAllText(out + ".ir", IR.format config) *)
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
                unimplementable (sprintf "Can not control inbound traffic from peer: %s" x)
            | IR.UncontrollablePeerPreference x -> 
                unimplementable (sprintf "Can not control inbound preference from peer: %s without MED or prepending" x)
    with Topology.InvalidTopologyException -> 
        error (sprintf "Invalid Topology: internal topology must be weakly connected")


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

    (* Get the topology *)
    let topo = Examples.topoDatacenterSmall()

    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p
        let pairs = Ast.makePolicyPairs ast topo
        match settings.Format with 
        | Args.IR ->
            let compiled = 
                pairs
                |> List.map (compileForSinglePrefix settings fullName)
            let foo = List.map (fun (prefix, dc) -> prefix) compiled
            ()
        | Args.Template -> ()

    0