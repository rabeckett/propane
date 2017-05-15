module Args

open DocoptNet
open System.IO

type T = 
   { PolFile : string option
     TopoFile : string option
     OutDir : string  
     Anycast : bool
     UseMed : bool
     UsePrepending : bool
     UseNoExport : bool
     Minimize : bool
     Parallel : bool
     Cbgp : bool
     Test : bool
     GenLinkTests : bool 
     GenPrefTests : bool
     Coverage : Option<int>
     Bench : bool
     Debug : bool
     DebugDir : string
     CheckFailures : bool
     Failures : Option<int>
     Verbose : bool
     Stats : bool
     Csv : bool
     CheckOnly : bool
     IsAbstract : bool
     IsTemplate : bool }

let currentDir = System.Environment.CurrentDirectory
let sep = string Path.DirectorySeparatorChar
let debugDir = ref (currentDir + sep + "debug" + sep)
let settings = ref None
let usage = """
Usage: propane [options]
       propane (--help | --version)

Options:
    -h, --help           Show this message.
    --version            Show the version of Propane.
    --policy FILE        Propane policy file.
    --topo FILE          Network topology file (xml).
    --output DIR         Specify output directory.
    --verbose            Display detailed information about fault-tolerance.
    --no-failures        Disable checks for aggregation safety
    --failures k         Guarantee k failure safety for aggregation.
    --check              Only check for correctness, don't generate configs.
    --parallel           Enable parallel compilation.
    --naive              Disable policy minimization.
    --stats              Display compilation statistics in readable format.
    --csv                Display compilation statistics in csv format.
    --anycast            Allow use of ip anycast.
    --med                Allow use of the BGP MED attribute.
    --prepending         Allow use of AS path prepending.
    --noexport           Allow use of the BGP no-export community.
    --cbgp               Generate C-BGP tests.
    --test               Run compiler unit tests.
    --bench              Generate benchmark policies.
    --debug              Output debugging information.
    --genLinkTests       Generate exhaustive link coverage test cases
    --genPrefTests       Generate preference coverage test cases
    --coverage k         k % random coverage of tests 
"""

let checkFile f = 
   let inline adjustFilePath f = 
      if Path.IsPathRooted(f) then f
      else currentDir + sep + f
   if File.Exists f then adjustFilePath f
   else 
      let f' = currentDir + sep + f
      if File.Exists f' then adjustFilePath f'
      else 
         printfn "Invalid file: %s" f
         exit 0

let exitUsage() = 
   printfn "%s" usage
   exit 0

let getFile (vo : ValueObject) = 
   if vo = null then None
   else Some(string vo |> checkFile)

let getDir (vo : ValueObject) = 
   if vo = null then None
   else Some(string vo)

let getFailures (vo : ValueObject) = 
   if vo = null then None
   else Some(vo.AsInt)


let getCoverage (vo : ValueObject) =
   if vo = null then None
   else Some(vo.AsInt)

let parse (argv : string []) : unit = 
   let d = Docopt()
   let vs = d.Apply(usage, argv, version = "Propane version 0.1", exit = true)
   if vs.["--help"].IsTrue then exitUsage()
   let outDir = getDir vs.["--output"]
   
   let outDir = 
      match outDir with
      | None -> currentDir + sep + "output"
      | Some d -> d
   debugDir := outDir + sep + "debug"
   let s = 
      { PolFile = getFile vs.["--policy"]
        TopoFile = getFile vs.["--topo"]
        OutDir = outDir
        Verbose = vs.["--verbose"].IsTrue
        CheckFailures = vs.["--no-failures"].IsFalse
        Failures = getFailures vs.["--failures"]
        CheckOnly = vs.["--check"].IsTrue
        Parallel = vs.["--parallel"].IsTrue
        Minimize = vs.["--naive"].IsFalse
        Stats = vs.["--stats"].IsTrue
        Csv = vs.["--csv"].IsTrue
        Anycast = vs.["--anycast"].IsTrue
        UseMed = vs.["--med"].IsTrue
        UsePrepending = vs.["--prepending"].IsTrue
        UseNoExport = vs.["--noexport"].IsTrue
        Cbgp = vs.["--cbgp"].IsTrue
        Test = vs.["--test"].IsTrue
        Coverage = getCoverage vs.["--coverage"]
        GenLinkTests = vs.["--genLinkTests"].IsTrue
        GenPrefTests = vs.["--genPrefTests"].IsTrue
        Bench = vs.["--bench"].IsTrue
        Debug = vs.["--debug"].IsTrue
        DebugDir = !debugDir
        IsAbstract = false
        IsTemplate = false } // these get set from the topology
   settings := Some s

let getSettings() = 
   match !settings with
   | Some s -> s
   | None -> 
      printfn "Error: no settings found"
      exit 0

let changeSettings s = settings := Some s