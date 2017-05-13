module Program

open System
open Util.Debug
open Util.Format
open CGraph

type Path = Set<CgState*CgState> 
type TestCases = Set<Path*Path>

let runUnitTests() = 
   writeFormatted (header "Running unit tests ")
   Topology.Test.run()
   Abgp.Test.run()

[<EntryPoint>]
let main argv = 
   let s = System.Diagnostics.Stopwatch()
   s.Start()
   ignore (Args.parse argv)
   let settings = Args.getSettings()
   if settings.Test then 
      runUnitTests()
      exit 0
   if settings.Bench then 
      Benchmark.generate()
      exit 0
   let (topoInfo, settings), t1 = 
      match settings.TopoFile with
      | None -> errorLine "No topology file specified, use --help to see options"
      | Some f -> Util.Profile.time Topology.readTopology f
   if settings.IsAbstract then AbstractAnalysis.checkWellformedTopology topoInfo
   if (settings.GenLinkTests || settings.GenPrefTests) then 
      System.IO.File.WriteAllText("ExpectedOutput.txt", "")
   match settings.Coverage with
   | None -> (); //Console.Write("none");
   | Some s ->  
      if (s < 0 || s > 100) then 
            errorLine "Invalid coverage amount, specify something between 0 and 100"
      else
            (); //Console.Write("coverage value in program is " + (string s));
   match settings.PolFile with
   | None -> errorLine "No policy file specified, use --help to see options"
   | Some polFile -> 
      Util.File.createDir settings.OutDir
      Util.File.createDir settings.DebugDir
      let ast, t2 = Util.Profile.time (Input.readFromFile topoInfo) polFile
      let polInfo, t3 = Util.Profile.time Ast.build ast

      let res, predToTests = Abgp.compileAllPrefixes polInfo
      let policy = polInfo.Policy.Head
      let _, reb, _ = policy
      let topo = reb.Topo()

      // where can i get topo from
      let routerNameToIp = TestGenerator.generateRouterIp topo

      let testPrintTime = 
            if settings.GenLinkTests || settings.GenPrefTests then Abgp.writeCBGPTests res.Abgp routerNameToIp topo predToTests
            else (int64 0);

      let sumCoverage = Map.fold (fun s k (_, c) -> if (c = -1.0) then s else s + c) 0.0 predToTests;
      let numKeys = Map.fold (fun s k (_, c) -> if (c = -1.0) then s else s + 1) 0 predToTests;
      Console.Write("number of predicates: " + (string numKeys));
      let cover = sumCoverage / (float numKeys);            

      if settings.CheckFailures then 
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
                  + sprintf 
                       "It may be possible to disconnect the prefix %s at location %s from the " 
                       (string p) x 
                  + sprintf "aggregate prefix %s at %s after %d failures. " (string agg) y (i + 1)
               
               let msg = 
                  if warn then 
                     sprintf 
                        "%sConsider using the --failures=k flag to specify a tolerable failure level." 
                        msg
                  else msg
               if warn then warning msg
               else error msg
         | _ -> ()
      let genStats, genTime = 
         if settings.CheckOnly then None, int64 0
         else 
            let stats, t = Util.Profile.time (Generate.generate res) topoInfo
            Some(stats), t
      s.Stop()
      if settings.Csv then 
         Stats.printCsv res.Stats genStats (t1 + t2 + t3) genTime testPrintTime cover s.ElapsedMilliseconds 
      else if settings.Stats then 
         Stats.print res.Stats genStats (t1 + t2 + t3) genTime testPrintTime cover s.ElapsedMilliseconds 
      else ()
   0