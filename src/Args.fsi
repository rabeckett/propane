module Args

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

/// Get the command-line settings
val getSettings : unit -> T
/// Update the settings
val changeSettings : T -> unit
/// Parse command line arguments and return the compiler settings
val parse : string [] -> unit