module Args

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
     Minimize: bool;
     Parallel: bool;
     Test: bool;
     Bench: bool;
     CheckEnter: bool;
     Debug: bool; 
     DebugDir: string;
     Failures: Failures;
     Stats: bool}

/// Get the command-line settings
val getSettings: unit -> T

/// Parse command line arguments and return the compiler settings
val parse: string[] -> unit