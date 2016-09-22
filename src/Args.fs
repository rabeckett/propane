module Args

open DocoptNet
open System.IO

type T = 
   { PolFile : string option
     TopoFile : string option
     OutDir : string
     IsAbstract : bool
     Anycast : bool
     UseMed : bool
     UsePrepending : bool
     UseNoExport : bool
     Minimize : bool
     Parallel : bool
     Test : bool
     Bench : bool
     Debug : bool
     DebugDir : string
     Failures : Option<int>
     Stats : bool
     CheckOnly : bool }

let currentDir = System.Environment.CurrentDirectory
let sep = string Path.DirectorySeparatorChar
let debugDir = ref (currentDir + sep + "debug" + sep)
let settings = ref None
let usage = """
Usage: propane [options]
       propane (--help | --version)

Options:
    -h, --help        Show this message.
    --version         Show the version of Propane.
    --policy FILE     Propane policy file.
    --topo FILE       Network topology file (xml).
    --output DIR      Specify output directory.
    --failures K      Guarantee k failure safety for aggregation.
    --check           Only check for correctness, don't generate configs
    --abstract        Compile for abstract topology.
    --parallel        Enable parallel compilation.
    --naive           Disable policy minimization.
    --stats           Display compilation statistics to stdout.
    --anycast         Allow use of ip anycast.
    --med             Allow use of the BGP MED attribute.
    --prepending      Allow use of AS path prepending.
    --noexport        Allow use of the BGP no-export community.
    --test            Run compiler unit tests.
    --bench           Generate benchmark policies.
    --debug           Output debugging information.
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
        Failures = getFailures vs.["--failures"]
        CheckOnly = vs.["--check"].IsTrue
        IsAbstract = vs.["--abstract"].IsTrue
        Parallel = vs.["--parallel"].IsTrue
        Minimize = vs.["--naive"].IsFalse
        Stats = vs.["--stats"].IsTrue
        Anycast = vs.["--anycast"].IsTrue
        UseMed = vs.["--med"].IsTrue
        UsePrepending = vs.["--prepending"].IsTrue
        UseNoExport = vs.["--noexport"].IsTrue
        Test = vs.["--test"].IsTrue
        Bench = vs.["--bench"].IsTrue
        Debug = vs.["--debug"].IsTrue
        DebugDir = !debugDir }
   settings := Some s

let getSettings() = 
   match !settings with
   | Some s -> s
   | None -> 
      printfn "Error: no settings found"
      exit 0