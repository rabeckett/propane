module Input

/// Parse an input propane file returning 
/// The input text: string [] 
/// The ast definitions 
/// The ast control constraints.
val readFromFile : Topology.TopoInfo -> string -> Ast.T