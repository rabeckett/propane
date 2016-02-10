open Common
open Common.Debug
open Common.Error
    

[<EntryPoint>]
let main argv =
    printfn ""
    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    if settings.Test then
        Test.run ()
        exit ()
    let fullName = settings.DebugDir + (Common.Option.getOrDefault "output" settings.OutFile)
    let topo = 
        match settings.TopoFile with 
        | None -> error ("\nNo topology file specified \nUse --topo:file compiler flag")
        | Some f -> Topology.readTopology f
    match settings.PolFile with 
    | None -> error ("\nNo policy file specified \nUse --pol:file compiler flag")
    | Some p ->
        let ast = Input.readFromFile p
        let aggs = Ast.getControlConstraints ast topo
        let pairs = Ast.makePolicyPairs ast topo
        let (ir, k, _) = IR.compileAllPrefixes fullName topo pairs aggs
        match k, settings.Failures with
        | Some (i, x, y), Args.Any -> 
            error (sprintf "\nRequired all-failure safety for aggregation, but only got %d-failure safety \nPossible weak point between %s and %s" i x y)
        | Some (i, x, y), Args.Concrete j when i < j ->
            error (sprintf "\nRequired %d-failure safety for aggregation, but only got %d-failure safety \nPossible weak point between %s and %s" j i x y)
        | _ -> ()
        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)
        (* TODO: another compilation step *)
        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () 

    0