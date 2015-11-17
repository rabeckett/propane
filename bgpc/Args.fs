module Args

type Spec = 
    | Unit of (unit -> unit)
    | String of (string -> unit)

type Format = 
    | IR 
    | Template
    | Graph

type T = 
    {PolFile: string;
     OutFile: string option;
     Format: Format;
     Debug: bool}

exception InvalidFormatException of string

let polFile = ref None
let outFile = ref None
let format = ref Template
let debug = ref false

let setFormat s = 
    match s with 
    | "IR" -> format := IR 
    | "Template" -> format := Template
    | "Graph" -> format := Graph
    | _ -> raise (InvalidFormatException s)

let usage = "Usage: bgpc.exe [options]"
let args = 
    [|("-pol", String (fun s -> polFile := Some s), "Policy file");
      ("-out", String (fun s -> outFile := Some s), "Output file");
      ("-format", String (fun s -> setFormat s), "Output format (Template, IR, Graph)");
      ("--debug", Unit (fun () -> debug := true), "Enable debugging information") |]

let printHelp () = 
    printfn "%s" usage
    for (param, _, descr) in args do 
        printfn "%s  %s" param descr
    printfn "--help  Display this message"

let exit () = 
    printHelp () 
    exit 0

let lookup s next i = 
    try 
        let (p, run, descr) = Array.find (fun (s',_,_) -> s=s') args
        match run with
        | Unit f -> f (); i + 1
        | String f ->
            match next with 
            | None ->
                printfn "Invalid usage: %s, %s" p descr
                exit ()
            | Some s' -> f s'; i + 2
    with
        | InvalidFormatException s ->
            printfn "Invalid format: %s" s
            exit ()
        |_ ->
            printfn "Unknown parameter: %s" s
            exit ()

let parse (argv: string[]) : T =
    if Array.isEmpty argv then 
        exit () 
    else
    try 
        Array.find (fun s -> s = "--help") argv |> ignore
        exit ()
    with _ -> 
        let mutable i = 0
        while (i < Array.length argv) do 
            let curr = argv.[i]
            let next = if (i = Array.length argv - 1) then None else Some argv.[i+1]
            i <- lookup curr next i
    match !polFile with
    | Some p -> 
        {PolFile = p; OutFile = !outFile; Format = !format; Debug = !debug}
    | None ->
        printfn "Missing policy input file"
        exit ()