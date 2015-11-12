module Input

/// Parse an input string to an abstract syntax tree
val readFromString: string -> Ast.T
   
/// Parse an input file to an abstract syntax tree
val readFromFile: string -> Ast.T


