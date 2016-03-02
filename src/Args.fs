module Args

open System.IO

type Spec = 
    | Unit of (unit -> unit)
    | String of (string -> unit)

type Target = 
    | Off
    | IR 
    | Template

type Failures = 
    | Any
    | Concrete of int

type T = 
    {PolFile: string option;
     TopoFile: string option;
     OutFile: string option;
     Target: Target;
     Anycast: bool;
     UseMed: bool;
     UsePrepending: bool;
     UseNoExport: bool;
     UseIBGP: bool;
     Minimize: bool;
     Parallel: bool;
     Test: bool;
     Bench: bool;
     CheckEnter: bool;
     Debug: bool; 
     DebugDir: string;
     Failures: Failures;
     Stats: bool}

exception InvalidArgException of string

let currentDir = System.Environment.CurrentDirectory
let sep = string Path.DirectorySeparatorChar

let polFile = ref None
let topoFile = ref None
let outFile = ref None
let target = ref IR
let anycast = ref false
let useMed = ref false
let usePrepending = ref false
let useNoExport = ref false
let useIBGP = ref false
let minimize = ref true
let isParallel = ref true
let test = ref false
let bench = ref false
let debug = ref false
let debugDir = ref (currentDir + sep + "debug" + sep)
let compression = ref true
let experiment = ref None
let checkEnter = ref false
let failures = ref Any
let stats = ref false

let settings = ref None

let cleanDir dir = 
    if Directory.Exists dir then
        Directory.Delete(dir, true)
    Directory.CreateDirectory(dir).Create()

let setOnOff var msg s =
    match s with 
    | "on" -> var := true
    | "off" -> var := false
    | _ -> raise (InvalidArgException (sprintf "Invalid %s value: %s" msg s))

let setTarget s = 
    match s with
    | "none" -> target := Off
    | "ir" -> target := IR 
    | "templ" -> target := Template
    | _ -> raise (InvalidArgException ("Invalid format value: " + s))

let setFailures s =
    match s with 
    | "any" -> failures := Any
    | _ -> 
        let i = 
            try int s
            with _ -> raise (InvalidArgException (sprintf "Invalid number: %s" s))
        if i < 0  then 
            raise (InvalidArgException ("Invalid number: " + s))
        failures := Concrete i

let inline adjustFilePath f = 
    if Path.IsPathRooted(f) then f 
    else currentDir + sep + f

let setFile f = 
    if File.Exists f then adjustFilePath f 
    else
        let f' = currentDir + sep + f
        if File.Exists f' then adjustFilePath f'
        else raise (InvalidArgException (sprintf "Invalid file: %s" f))

let setDebugDir s =
    if Path.IsPathRooted(s) 
    then debugDir := s + sep 
    else debugDir := currentDir + sep + s + sep

let usage = "Usage: propane [options]"
let args = 
    [|("-pol", String (fun s -> polFile := Some (setFile s)), "Policy file");
      ("-topo", String (fun s -> topoFile := Some (setFile s)), "Topology file");
      ("-out", String (fun s -> outFile := Some s), "Output file");
      ("-failures:any|n", String setFailures, "Failure safety for aggregation (default any)");
      ("-anycast:on|off", String (setOnOff anycast "anycast"), "Allow anycast (default off)");
      ("-med:on|off", String (setOnOff useMed "MED"), "Use MED attribute (default off)");
      ("-prepending:on|off", String (setOnOff usePrepending "prepending"), "Use AS path prepending (default off)");
      ("-no-export:on|off", String (setOnOff useNoExport "no-export"), "Use no-export community (default off)");
      ("-ibgp:on|off", String (setOnOff useIBGP "ibgp"), "Assume IBGP configuration (default off)");
      ("-minimize:on|off", String (setOnOff minimize "minimize"), "Minimize configuration (default on)");
      ("-parallel:on|off", String (setOnOff isParallel "parallel"), "Parallelize compilation (default on)");
      ("-target:none|ir|templ", String setTarget, "Compilation target");
      ("-stats:on|off", String (setOnOff stats "stats"), "Display performance statistics to stdout (default off)");
      ("-checkenter:on|off", String (setOnOff checkEnter "check enter"), "Check traffic entry conditions to network");
      ("-debug:on:off", String (setOnOff debug "debug"), "Log/Save Debugging information (default off)");
      ("-debug-dir", String setDebugDir, "Debugging directory (default 'debug')");
      ("-test", Unit (fun () -> test := true), "Run unit tests");
      ("-bench", Unit (fun () -> bench := true), "Generate benchmark files");
      ("-help", Unit (fun () -> ()), "Display this message");
    |]

let printHelp () = 
    let (s,_,_) = Array.maxBy (fun (s,_, _) -> String.length s) args
    let max = String.length s
    printfn "\n%s" usage
    for (param, _, descr) in args do
        let nspaces = max - (String.length param) + 3
        let spaces = String.replicate nspaces " "
        printfn "%s%s%s" param spaces descr
    printfn ""

let exit () = 
    printHelp () 
    exit 0

let lookup (s: string) next i = 
    try 
        let arr = s.Split(':')
        let s = arr.[0]
        match Array.tryFind (fun (s' : string,_,_) -> s = s'.Split(':').[0]) args with
        | None -> 
            printfn "\nUnrecognized option: %s" s
            exit ()
        | Some (p,run,descr) ->
            match run with
            | Unit f -> f (); i + 1
            | String f ->
                if arr.Length > 1 then 
                    let s' = arr.[1]
                    f s'; i + 1
                else
                    match next with 
                    | None ->
                        printfn "\nInvalid usage: %s, %s" p descr
                        exit ()
                    | Some s' -> f s'; i + 2
    with InvalidArgException msg -> 
        printfn "\n%s" msg
        exit ()

let parse (argv: string[]) : unit =
    if Array.isEmpty argv then 
        exit () 
    else
    try 
        Array.find (fun s -> s = "-help") argv |> ignore
        exit ()
    with _ -> 
        let mutable i = 0
        while (i < Array.length argv) do 
            let curr = argv.[i]
            let next = if (i = Array.length argv - 1) then None else Some argv.[i+1]
            i <- lookup curr next i
    cleanDir !debugDir
    settings := 
        Some {PolFile = !polFile; 
              TopoFile = !topoFile;
              OutFile = !outFile;
              Anycast = !anycast;
              UseMed = !useMed; 
              UsePrepending = !usePrepending; 
              UseNoExport = !useNoExport;
              UseIBGP = !useIBGP; 
              Minimize = !minimize;
              Parallel = !isParallel;
              Target = !target; 
              Test = !test; 
              Bench = !bench;
              CheckEnter = !checkEnter;
              Debug = !debug; 
              DebugDir = !debugDir;
              Failures = !failures;
              Stats = !stats}

let getSettings () = 
    match !settings with
    | Some s -> s
    | None -> 
        printfn "Error: no settings found"
        exit ()
