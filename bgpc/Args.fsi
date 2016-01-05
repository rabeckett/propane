module Args

type Format = 
    | IR 
    | Template

type T = 
    {PolFile: string option;
     OutFile: string option;
     Format: Format;
     UseMed: bool;
     UsePrepending: bool;
     UseNoExport: bool;
     Test: bool;
     Debug: int; 
     DebugDir: string}

/// The command-line settings
val getSettings: unit -> T

/// Parse command line arguments and return the compiler settings
/// The default settings are to format with IR, 
/// Debugging off, and no output file.
/// The input policy file is required.
val parse: string[] -> unit