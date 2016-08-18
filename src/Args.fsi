module Args

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
    CheckEnter : bool
    Debug : bool
    DebugDir : string
    Failures : Option<int>
    Stats : bool }

/// Get the command-line settings
val getSettings : unit -> T
/// Parse command line arguments and return the compiler settings
val parse : string [] -> unit