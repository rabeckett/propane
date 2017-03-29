module Program

open System
open Util.Debug
open Util.Format

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
   if settings.GenTests then System.IO.File.WriteAllText("solutions.txt", "")
   match settings.PolFile with
   | None -> errorLine "No policy file specified, use --help to see options"
   | Some polFile -> 
      Util.File.createDir settings.OutDir
      Util.File.createDir settings.DebugDir
      let ast, t2 = Util.Profile.time (Input.readFromFile topoInfo) polFile
      let polInfo, t3 = Util.Profile.time Ast.build ast

      let res, predToTests = Abgp.compileAllPrefixes polInfo

      // where can i get topo from
      let routerNameToIp = TestGenerator.generateRouterIp topo topoInfo

      // write the tests into CBGP file
      let createTest (pred: Route.Predicate) (tests : TestCases) = 
        for i in 0.. Seq.length tests - 1 do
            let t = Seq.item i tests
            let outputFile = "test" + (Route.toString pred) + (string) i + ".cli"
            TestGenerator.writeTopoCBGP topo outputFile; // writes physical topology to all testfiles
            // output cbgp router configuration instructions for routers in the path
            for (src, dest) in t do
            let s = Abgp.getCBGPConfig res.Abgp src routerNameToIp
            File.AppendAllText(outputFile, s);
            //TODO: traceroute command that would output the path that the traffic took
      Map.iter createTest predToTests;

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
         Stats.printCsv res.Stats genStats (t1 + t2 + t3) genTime s.ElapsedMilliseconds
      else if settings.Stats then 
         Stats.print res.Stats genStats (t1 + t2 + t3) genTime s.ElapsedMilliseconds
      else ()
   0