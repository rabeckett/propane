module Args

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
     Parallel: bool;
     Test: bool;
     CheckEnter: bool;
     Debug: int; 
     DebugDir: string;
     Failures: Failures;
     Stats: bool}

/// Get the command-line settings
val getSettings: unit -> T

/// Parse command line arguments and return the compiler settings
val parse: string[] -> unit