module Program

open Common
open Common.Debug
open Common.Color
open System
    


[<EntryPoint>]
let main argv =
    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    if settings.Test then
        Test.run ()
        exit 0
    let fullName = settings.DebugDir + (Common.Option.getOrDefault "output" settings.OutFile)
    let topo = 
        match settings.TopoFile with 
        | None -> error "No topology file specified, use -topo:file compiler flag"
        | Some f -> Topology.readTopology f
    match settings.PolFile with 
    | None -> error "No policy file specified, use -pol:file compiler flag"
    | Some p ->
        let ast = Input.readFromFile p
        let aggs = Ast.getControlConstraints ast topo
        let pairs = Ast.makePolicyPairs ast topo
        let (ir, k, _) = IR.compileAllPrefixes fullName topo pairs aggs
        match k, settings.Failures with
        | Some (i, x, y), Args.Any -> 
            let msg = 
                sprintf "Required black-hole safety for aggregates under all failures, " + 
                sprintf "but could only prove failure safety for up to %d failures. " i +
                sprintf "It may be possible disconnect prefix at %s from aggregate at %s after %d failures. " x y (i+1) +
                sprintf "Consider using the -failures:n flag to specify a tolerable failure level."
            warning msg
        | Some (i, x, y), Args.Concrete j when i < j ->
            let msg = 
                sprintf "Required black-hole safety for aggregates under any combination of %d failures, " j +
                sprintf "but could only prove failure safety for up to %d failures. " i +
                sprintf "It may be possible to disconnect prefix at %s from aggregate at %s after %d failures" x y (i+1) +
                sprintf "Consider using the -failures:n flag to specify a tolerable failure level."
            warning msg
        | _ -> ()
        match settings.OutFile with
        | None -> ()
        | Some out -> System.IO.File.WriteAllText(out + ".ir", IR.format ir)
        (* TODO: another compilation step *)
        match settings.Format with 
        | Args.IR -> ()
        | Args.Template -> () 

    0