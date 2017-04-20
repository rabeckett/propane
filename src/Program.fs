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
      System.IO.File.WriteAllText("solutions.txt", "")
      System.IO.File.WriteAllText("ExpectedOutput.txt", "")
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

      // write the tests into CBGP file
      let mutable j = 0
      let createTest (pred: Route.Predicate) (tests : TestCases) = 
            //Console.Write("number of tests for thsi pred: " + (string (Set.count tests)))
            let predStr = 
                  let (Route.TrafficClassifier(pref, _)) = List.head (Route.trafficClassifiers pred)
                  let s = (string) pref
                  if (String.exists (fun c -> c = 'l') s) then (s.Substring (0, 9)) 
                  else s
            for i in 0.. Seq.length tests - 1 do
                  let (t, e) = Seq.item i tests
                  let outputFile = "test" + (string) j + ".cli"
                  j <- j + 1
                  if not (Seq.isEmpty t) then
                        TestGenerator.writeTopoCBGP topo outputFile // writes physical topology to all testfiles
                  
                  // get Ipaddress for a given node in the testGraph
                  let getIp (v : CgState) =
                        let routerName = v.Node.Loc
                        Map.find routerName routerNameToIp

                  // get Ipaddress for a given node in the testGraph
                  let getAsn (v : Topology.Node) = v.Loc

                  let getMap (neighbors : seq<Topology.Node>) =
                        let mutable neighborsToNode = Map.empty
                        for n in neighbors do
                              let routerName = n.Loc
                              neighborsToNode <- Map.add routerName n neighborsToNode
                        neighborsToNode

                  //Console.Write(outputFile + "\n");
                  // create map with vertex to its peers in test topology
                  let mutable vertexToPeers = Map.empty
                  let mutable testVerticesInOrder = Map.empty
                  let mutable startingVertex = ""
                  for (src, dest) in t do
                        // add dest to src's neighbor list
                        System.IO.File.AppendAllText("solutions.txt", outputFile + "\n");
                        System.IO.File.AppendAllText("solutions.txt", (string src) + " " + (string dest) + "\n");
                        let neighbors =
                              if (Map.containsKey src.Node vertexToPeers) then Map.find src.Node vertexToPeers
                              else Set.empty
                        let newneighbors = 
                              if (Topology.isTopoNode dest.Node) then Set.add dest.Node neighbors
                              else neighbors
                        if (Topology.isTopoNode src.Node) then
                              vertexToPeers <- Map.add src.Node newneighbors vertexToPeers
                        else ()                
                        //add src to dest's neighbor list
                        let destneighbors =
                              if (Map.containsKey dest.Node vertexToPeers) then Map.find dest.Node vertexToPeers
                              else Set.empty
                        let destnewneighbors = 
                              if (Topology.isTopoNode src.Node) then Set.add src.Node destneighbors
                              else destneighbors
                        if (Topology.isTopoNode dest.Node) then
                              vertexToPeers <- Map.add dest.Node destnewneighbors vertexToPeers
                        else ()

                  let mutable lastRouter = "0.0.0.0"
                  let mutable lastAsn = "0"
                  let mutable lastCGNode = 
                        {
                              Id = 0
                              State = 0
                              Accept = (int16 0)
                              Node = new Topology.Node("", Topology.Start)
                        }
                  for (src, dest) in e do
                        // track teh vertices in order to geenrate exepcted output
                        if (Topology.isTopoNode dest.Node) then
                              if (Topology.isTopoNode src.Node) then
                                    testVerticesInOrder <- Map.add dest.Node.Loc src.Node.Loc testVerticesInOrder
                        else
                              startingVertex <- src.Node.Loc
                              lastRouter <- getIp src
                              lastAsn <- src.Node.Loc
                              lastCGNode <- src
                  if not (Seq.isEmpty t) then
                      TestGenerator.geteBGPStaticRoutes vertexToPeers routerNameToIp outputFile
                  
                  // output cbgp router configuration instructions for routers in the path
                  let mutable lessPrefLastRouter = "0.0.0.0"
                  let mutable lessPrefLastAsn = "0"
                  let mutable verticesSoFar = Set.empty
                  for (src, dest) in t do
                        //System.IO.File.AppendAllText(outputFile, Topology.router src.Node.Loc topoInfo);
                        if (Topology.isTopoNode dest.Node && (not (Set.contains dest.Node verticesSoFar))) then                              
                              let neighbors = Seq.map getAsn (Map.find dest.Node vertexToPeers)
                              let neighborsToNode = getMap (Map.find dest.Node vertexToPeers)
                              let isStart = not (Topology.isTopoNode src.Node) 
                              let s = Abgp.getCBGPConfig res.Abgp dest isStart predStr neighbors routerNameToIp neighborsToNode
                              System.IO.File.AppendAllText(outputFile, s);
                              verticesSoFar <- Set.add dest.Node verticesSoFar;
                        if (not (Topology.isTopoNode dest.Node) && src <> lastCGNode) then 
                              lessPrefLastRouter <- getIp src
                              lessPrefLastAsn <- src.Node.Loc
                  if not (Seq.isEmpty t) then
                        System.IO.File.AppendAllText(outputFile, "\nsim run\n\n");
                        System.IO.File.AppendAllText(outputFile, "\nbgp router " + lastRouter + " record-route " + predStr)
                        if (settings.GenPrefTests && lessPrefLastRouter <> lastRouter)  then
                              // expect Failure
                              System.IO.File.AppendAllText(outputFile, "\nbgp router " + lessPrefLastRouter + " record-route " + predStr)
                        System.IO.File.AppendAllText("solutions.txt", outputFile + " last router is " + lastAsn + "\n");

                  if not (Seq.isEmpty e) then
                        // print out the reference output in a separate file
                        let refOutputFile = "ExpectedOutput.txt"
                        System.IO.File.AppendAllText(refOutputFile, outputFile + " " + lastRouter + "\t" + predStr + "\t" + "SUCCESS\t");
                        while (Map.containsKey startingVertex testVerticesInOrder) do
                              System.IO.File.AppendAllText(refOutputFile, startingVertex + " ");
                              startingVertex <- Map.find startingVertex testVerticesInOrder
                        System.IO.File.AppendAllText(refOutputFile, startingVertex + "\n");

                        // for preference coverage, look at the less preferred path and see if the end router is on the preferred path or not
                        // accordingly verify that it has or doesn't have a path to the origin vertex
                        if (settings.GenPrefTests && lessPrefLastRouter <> lastRouter) then
                              if (not(Map.containsKey lessPrefLastAsn testVerticesInOrder)) then
                                    System.IO.File.AppendAllText(refOutputFile, outputFile + " " + lessPrefLastRouter + "\t" + predStr + "\t" + "UNREACHABLE\t\n");
                              // expect Failure
                              else 
                                    startingVertex <- lessPrefLastAsn
                                    System.IO.File.AppendAllText(refOutputFile, outputFile + " " + lessPrefLastRouter + "\t" + predStr + "\t" + "SUCCESS\t");
                                    while (Map.containsKey startingVertex testVerticesInOrder) do
                                          System.IO.File.AppendAllText(refOutputFile, startingVertex + " ");
                                          startingVertex <- Map.find startingVertex testVerticesInOrder
                                    System.IO.File.AppendAllText(refOutputFile, startingVertex + "\n");

      if settings.GenLinkTests then 
            Map.iter createTest predToTests;
      else 
            if settings.GenPrefTests then 
                  Map.iter createTest predToTests;
            else
                  ();

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