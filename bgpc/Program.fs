open Common
open Common.Debug
open Common.Error
    

[<EntryPoint>]
let main argv = 
    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    if settings.Test then
        Test.run ()
        exit 0
    let fullName = settings.DebugDir + (Common.Option.getOrDefault "output" settings.OutFile)
    let topo = Examples.topoDatacenterSmall()
    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p
        let aggs = Ast.getControlConstraints ast topo
        let pairs = Ast.makePolicyPairs ast topo
        let (ir, k, _) = IR.compileAllPrefixes fullName topo pairs aggs
        match k, settings.Failures with
        | Some i, Args.Any -> 
            error (sprintf "required all-failure safety for aggregation, but only got %d-failure safety" i)
        | Some i, Args.Concrete j when i < j ->
            error (sprintf "required %d-failure safety for aggregation, but only got %d-failure safety" j i)
        | _ -> ()
        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)
        (* TODO: another compilation step *)
        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () 

    0