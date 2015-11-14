module Args

type Format = 
    | IR 
    | Template
    | Graph

type T = 
    {PolFile: string;
     OutFile: string option;
     Format: Format;
     Debug: bool}

/// Parse command line arguments and return the compiler settings
/// The default settings are to format with Template, 
/// Debugging off, and no output file.
/// The input policy file is required.
val parse: string[] -> T