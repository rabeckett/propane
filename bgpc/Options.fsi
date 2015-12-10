module Options

type Format = 
    | IR 
    | Template
    | Graph

type T = 
    {PolFile: string option;
     OutFile: string option;
     Format: Format;
     Test: bool}

/// If we are in debug mode
val debug: bool ref

/// Parse command line arguments and return the compiler settings
/// The default settings are to format with Template, 
/// Debugging off, and no output file.
/// The input policy file is required.
val parse: string[] -> T