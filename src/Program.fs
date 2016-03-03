module Program

open Util
open Util.Debug
open Util.Format
open System
open FastPredicate


let runUnitTests () = 
    writeFormatted (header "Running unit tests ")
    Topology.Test.run () 
    Regex.Test.run () 
    Predicate.Test.run ()
    Abgp.Test.run ()

let total xs = 
    xs 
    |> Array.map float 
    |> Array.sum

let displayStats (stats: Abgp.Stats) = 
    printfn ""
    printfn "Total PG construction time (sec):  %f" (total stats.PerPrefixBuildTimes / 1000.0)
    printfn "Total PG Minimization time (sec):  %f" (total stats.PerPrefixMinTimes / 1000.0)
    printfn "Total Find Ordering time (sec):    %f" (total stats.PerPrefixOrderTimes / 1000.0)
    printfn "Total Generate Config time (sec):  %f" (total stats.PerPrefixGenTimes / 1000.0)
    printfn "Total Config Min time (sec):       %f" (float stats.MinTime / 1000.0)
    printfn ""

[<EntryPoint>] 
let main argv =
    let pb = PredicateBuilder()
    let p1 = Prefix(0,1,0,2,24,Range(24,32))
    let p2 = Prefix(0,1,0,1,16,Range(16,32))
    let x = pb.Prefix p1 
    let y = pb.Prefix p2
    printfn "%s" (pb.ToString(x))
    printfn "%s" (pb.ToString(pb.Not x))
    exit 0

    ignore (Args.parse argv)
    let settings = Args.getSettings ()
    if settings.Test then runUnitTests (); exit 0
    if settings.Bench then Benchmark.generate (); exit 0
    let fullName = settings.DebugDir + (Util.Option.getOrDefault "output" settings.OutFile)
    let topoInfo = 
        match settings.TopoFile with 
        | None -> error "No topology file specified, use -topo:file compiler flag"
        | Some f -> Topology.readTopology f
    match settings.PolFile with 
    | None -> error "No policy file specified, use -pol:file compiler flag"
    | Some polFile ->
        let (lines, defs, cs) = Input.readFromFile polFile
        let ast : Ast.T = {Input = lines; TopoInfo = topoInfo; Defs = defs; CConstraints = cs}
        let polInfo = Ast.build ast
        let res = Abgp.compileAllPrefixes fullName polInfo
        match res.AggSafety, settings.Failures with
        | Some safetyInfo, _ -> 
            let i = safetyInfo.NumFailures
            let bad, warn = 
                match settings.Failures with 
                | Args.Any -> true, true
                | Args.Concrete j -> i < j, false
            if bad then
                let x = Topology.router safetyInfo.PrefixLoc topoInfo
                let y = Topology.router safetyInfo.AggregateLoc topoInfo
                let p = safetyInfo.Prefix
                let agg = safetyInfo.Aggregate
                let msg = 
                    sprintf "Could only prove aggregation black-hole safety for up to %d failures. " i +
                    sprintf "It may be possible to disconnect the prefix %s at location %s from the " (string p) x +
                    sprintf "aggregate prefix %s at %s after %d failures. " (string agg) y (i+1) +
                    sprintf "Consider using the -failures:n flag to specify a tolerable failure level."
                if warn then warning msg else error msg
        | _ -> ()
        if settings.Stats then 
            displayStats res.Stats
        if settings.Target <> Args.Off then          
            match settings.OutFile, settings.Target with
            | None, _ -> ()
            | Some out, Args.IR -> System.IO.File.WriteAllText(out + ".ir", Abgp.format res.Abgp)
            | Some _, _ -> ()
    0