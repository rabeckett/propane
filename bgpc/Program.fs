open Common
open Common.Debug
open Common.Error


[<EntryPoint>]
let main argv =
    (* Parse command line settings *)
    ignore (Args.parse argv)
    let settings = Args.getSettings ()

    (* Run unit tests if in test mode *)
    if settings.Test then
        Test.run ()
        exit 0

    (* Set debugging output file *)
    let fullName = settings.DebugDir + (Common.Option.getOrDefault "output" settings.OutFile)

    (* Get the topology *)
    let topo = Examples.topoDatacenterSmall()

    match settings.PolFile with 
    | None -> error ("No policy file specified")
    | Some p ->
        let ast = Input.readFromFile p

        let aggs = Ast.getControlConstraints ast topo
        let pairs = Ast.makePolicyPairs ast topo

        printfn "Aggregates: %A" aggs
        printfn "Pairs: %A" pairs

        let ir = IR.compileAllPrefixes fullName topo pairs aggs

        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)

        (* TODO: another compilation step *)
        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () 

    0