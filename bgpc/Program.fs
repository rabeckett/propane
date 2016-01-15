open Common
open Common.Debug
open Common.Error

let datacenter () = 
    let (topo, _, _) = Examples.dataCenter [(2,2); (2,2); (2,1)]
    printfn "number of vertices: %d" (Seq.length topo.Vertices)
    printfn "number of edges: %d" (Seq.length topo.Edges)
    for v in topo.Vertices do 
        printfn "  %s" v.Loc


[<EntryPoint>]
let main argv =
    datacenter ()
    exit 0
    
    // Parse command line settings
    ignore (Args.parse argv)
    let settings = Args.getSettings ()

    // Run unit tests if in test mode
    if settings.Test then
        Test.run ()
        exit 0

    // Set debugging output file
    let fullName = settings.DebugDir + (Common.Option.getOrDefault "output" settings.OutFile)

    // Get the topology
    let topo = Examples.topoDatacenterSmall()

    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p

        let aggs = Ast.getControlConstraints ast topo
        let pairs = Ast.makePolicyPairs ast topo

        printfn "constraints: %A" aggs

        let ir = IR.compileAllPrefixes fullName topo pairs aggs

        // printfn "IR: %A" ir

        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)

        (* TODO: another compilation step *)
        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () 

    0