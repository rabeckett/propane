module Options

type Format = 
    | IR 
    | Template

type T = 
    {PolFile: string option;
     OutFile: string option;
     Format: Format;
     Test: bool}

/// Debug level 0-3 (default 0, no information)
val debug: int ref

/// Debug directory 
val debugDir: string

/// Parse command line arguments and return the compiler settings
/// The default settings are to format with Template, 
/// Debugging off, and no output file.
/// The input policy file is required.
val parse: string[] -> T