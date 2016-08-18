module Program

open System
open Util.Debug
open Util.Format

let runUnitTests() = 
  writeFormatted (header "Running unit tests ")
  Topology.Test.run()
  Regex.Test.run()
  //Route.Test.run ()
  Abgp.Test.run()

let total xs = 
  xs
  |> Array.map float
  |> Array.sum

let printStats (stats : Abgp.Stats) = 
  printfn ""
  printfn "Total PG construction time (sec):  %f" (total stats.PerPrefixBuildTimes / 1000.0)
  printfn "Total PG Minimization time (sec):  %f" (total stats.PerPrefixMinTimes / 1000.0)
  printfn "Total Find Ordering time (sec):    %f" (total stats.PerPrefixOrderTimes / 1000.0)
  printfn "Total Generate Config time (sec):  %f" (total stats.PerPrefixGenTimes / 1000.0)
  printfn "Total Config Min time (sec):       %f" (float stats.MinTime / 1000.0)
  printfn ""

[<EntryPoint>]
let main argv = 
  ignore (Args.parse argv)
  let settings = Args.getSettings()
  if settings.Test then 
    runUnitTests()
    exit 0
  if settings.Bench then 
    Benchmark.generate()
    exit 0
  let topoInfo = 
    match settings.TopoFile with
    | None -> errorLine "No topology file specified, use --help to see options"
    | Some f -> Topology.readTopology f
  match settings.PolFile with
  | None -> errorLine "No policy file specified, use --help to see options"
  | Some polFile -> 
    let (lines, defs, cs) = Input.readFromFile polFile
    
    let ast : Ast.T = 
      { Input = lines
        TopoInfo = topoInfo
        Defs = defs
        CConstraints = cs }
    
    let polInfo = Ast.build ast
    let res = Abgp.compileAllPrefixes polInfo
    match res.AggSafety with
    | Some safetyInfo -> 
      let i = safetyInfo.NumFailures
      
      let bad, warn = 
        match settings.Failures with
        | None -> true, true
        | Some j -> i < j, false
      if bad then 
        let x = Topology.router safetyInfo.PrefixLoc topoInfo
        let y = Topology.router safetyInfo.AggregateLoc topoInfo
        let p = safetyInfo.Prefix
        let agg = safetyInfo.Aggregate
        let msg = 
          sprintf "Could only prove aggregation black-hole safety for up to %d failures. " i 
          + sprintf "It may be possible to disconnect the prefix %s at location %s from the " 
              (string p) x 
          + sprintf "aggregate prefix %s at %s after %d failures. " (string agg) y (i + 1) 
          + sprintf "Consider using the -failures:n flag to specify a tolerable failure level."
        if warn then warning msg
        else error msg
    | _ -> ()
    if settings.Stats then printStats res.Stats
    Generate.generate settings.OutDir res
  0