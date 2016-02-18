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
    let topoInfo = 
        match settings.TopoFile with 
        | None -> error "No topology file specified, use -topo:file compiler flag"
        | Some f -> Topology.readTopology f
    match settings.PolFile with 
    | None -> error "No policy file specified, use -pol:file compiler flag"
    | Some p ->
        let (lines, defs, cs) = Input.readFromFile p
        let ast : Ast.T = {Input = lines; TopoInfo = topoInfo; Defs = defs; CConstraints = cs}
        let aggs = Ast.getControlConstraints ast topoInfo.Graph
        let pairs = Ast.makePolicyPairs ast topoInfo.Graph
        let (ir, k, _) = IR.compileAllPrefixes fullName topoInfo.Graph pairs aggs
        match k, settings.Failures with
        | Some (i, x, y, p), _ ->
            let bad = 
                match settings.Failures with 
                | Args.Any -> true
                | Args.Concrete j -> i < j 
            if bad then
                let x = Map.findKey (fun _ v -> string v = x) topoInfo.AsnMap
                let y = Map.findKey (fun _ v -> string v = y) topoInfo.AsnMap
                let pfxStr = string p.Head
                let msg = 
                    sprintf "Could only prove aggregation black-hole safety for up to %d failures. " i +
                    sprintf "It may be possible to disconnect the prefix %s at location %s from the " pfxStr x +
                    sprintf "aggregate at %s after %d failures. " y (i+1) +
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