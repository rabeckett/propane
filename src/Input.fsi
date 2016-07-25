module Input

/// Parse an input propane file returning 
/// The input text: string [] 
/// The ast definitions 
/// The ast control constraints.
val readFromFile : string -> string [] * Ast.Definitions * Ast.ControlConstraints
