module Args

type Spec = 
    | Unit of (unit -> unit)
    | String of (string -> unit)

type Format = 
    | IR 
    | Template

type Failures = 
    | Any
    | Concrete of int

type T = 
    {PolFile: string option;
     TopoFile: string option;
     OutFile: string option;
     Format: Format;
     UseMed: bool;
     UsePrepending: bool;
     UseNoExport: bool;
     Test: bool;
     CheckEnter: bool;
     Debug: int; 
     DebugDir: string;
     Failures: Failures;
     Stats: bool}

exception InvalidArgException of string

let polFile = ref None
let topoFile = ref None
let outFile = ref None
let format = ref IR
let useMed = ref false
let usePrepending = ref false
let useNoExport = ref false
let test = ref false
let debug = ref 0
let debugDir = ref ("debug" + string System.IO.Path.DirectorySeparatorChar)
let compression = ref true
let experiment = ref None
let checkEnter = ref false
let failures = ref Any
let stats = ref false

let settings = ref None


let cleanDir dir = 
    if System.IO.Directory.Exists dir then
        System.IO.Directory.Delete(dir, true)
    System.IO.Directory.CreateDirectory(dir).Create()

let setMED s = 
    match s with 
    | "on" -> useMed := true
    | "off" -> useMed := false
    | _ -> raise (InvalidArgException ("Invalid MED value: " + s))

let setPrepending s = 
    match s with 
    | "on" -> usePrepending := true
    | "off" -> usePrepending := false
    | _ -> raise (InvalidArgException ("Invalid AS path prepending value: " + s))

let setNoExport s =
    match s with 
    | "on" -> useNoExport := true
    | "off" -> useNoExport := false
    | _ -> raise (InvalidArgException ("Invalid No Export value: " + s))

let setFormat s = 
    match s with 
    | "IR" -> format := IR 
    | "Template" -> format := Template
    | _ -> raise (InvalidArgException ("Invalid format value: " + s))

let setDebugDir s =
    debugDir := s + string System.IO.Path.DirectorySeparatorChar

let setDebug s = 
    let i = 
        try int s 
        with _ -> raise (InvalidArgException (sprintf "Invalid number: %s" s))
    if i < 0 || i > 3 then 
        raise (InvalidArgException ("Invalid debug level: " + s))
    debug := i

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

let setCheckEnter s = 
    match s with 
    | "on" -> checkEnter := true
    | "off" -> checkEnter := false
    | _ -> raise (InvalidArgException ("Invalid checkenter argument: " + s))

let setStats s = 
    match s with 
    | "csv" -> stats := true
    | "none" -> stats := false
    | _ -> raise (InvalidArgException (sprintf "Invalid stats value: %s" s))

let usage = "Usage: bgpc.exe [options]"
let args = 
    [|("--pol", String (fun s -> polFile := Some s), "Policy file");
      ("--topo", String (fun s -> topoFile := Some s), "Topology file");
      ("--out", String (fun s -> outFile := Some s), "Output file");
      ("--failures:any|n", String setFailures, "Failure safety for aggregation (default any)");
      ("--med:on|off", String setMED, "Use MED attribute (default off)");
      ("--prepending:on|off", String setPrepending, "Use AS path prepending (default off)");
      ("--no-export:on|off", String setNoExport, "Use no-export community (default off)");
      ("--format:IR|Templ", String setFormat, "Output format (IR, Template)");
      ("--stats:csv|none", String setStats, "Display performance statistics to stdout (default none)");
      ("--checkenter:on|off", String (fun s -> setCheckEnter s), "Run experiment for dc or core");
      ("--test", Unit (fun () -> test := true), "Run unit tests");
      ("--debug-dir", String setDebugDir, "Debugging directory (default 'debug')");
      ("--debug:0|1|2|3", String setDebug, "Debug level (default lowest 0)");
      ("--help", Unit (fun () -> ()), "Display this message");
    |]

let printHelp () = 
    let (s,_,_) = Array.maxBy (fun (s,_, _) -> String.length s) args
    let max = String.length s
    printfn "%s" usage
    for (param, _, descr) in args do
        let nspaces = max - (String.length param) + 3
        let spaces = String.replicate nspaces " "
        printfn "%s%s%s" param spaces descr

let exit () = 
    printHelp () 
    exit 0

let lookup (s: string) next i = 
    try 
        let arr = s.Split(':')
        let s = arr.[0]
        match Array.tryFind (fun (s' : string,_,_) -> s = s'.Split(':').[0]) args with
        | None -> 
            printfn "Unrecognized option: %s" s
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
                        printfn "Invalid usage: %s, %s" p descr
                        exit ()
                    | Some s' -> f s'; i + 2
    with InvalidArgException msg -> 
        printfn "%s" msg
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
              UseMed = !useMed; 
              UsePrepending = !usePrepending; 
              UseNoExport = !useNoExport; 
              Format = !format; 
              Test = !test; 
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
