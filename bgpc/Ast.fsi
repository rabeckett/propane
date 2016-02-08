module Ast

/// Ast-based predicate, which is converted to a Predicate.T
type Predicate =
    | True
    | False
    | Prefix of uint32 * uint32 * uint32 * uint32 * uint32 option
    | Community of uint32 * uint32 
    | Or of Predicate * Predicate
    | And of Predicate * Predicate
    | Not of Predicate

/// Ast-based Regular expression, which is converted to a Regex.T
type Re = 
    | Empty
    | Concat of Re * Re
    | Union of Re * Re 
    | Inter of Re * Re 
    | Difference of Re * Re
    | Negate of Re
    | Star of Re
    | Shr of Re * Re
    | Ident of string * Re list

/// Ast expression for control constraint parameters e.g., aggregate(10.0.0.0/16, in->out)
type Expr =
    | PredicateExpr of Predicate
    | LinkExpr of Re * Re
    | IntLiteral of uint32
    | IdentExpr of string

/// Individual task definition with a name and ordered path constraints
type Task =
    {Name: string;
     PConstraints: (Predicate * Re) list}

/// Individual control constraint 
type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32

/// Final pairs of predicate, regular preferences after merging tasks
type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

/// Value of a definition
type Def = 
    | DRegex of Re
    | DExpr of Expr
    | DBuiltin

/// Ast type with final definitions, control contraint, task definitions, and the final policy
type T = 
    {Defs: Map<string, Def>;
     CConstraints: (string * Expr list) list;
     Tasks: Task list;
     Policy: Re}

/// Parse control constraint information w.r.t the topology
val getControlConstraints: T -> Topology.T -> CConstraint list

/// Build the final predicate, preference pairs by 
/// merging tasks and evaluating the main policy
val makePolicyPairs: T -> Topology.T -> PolicyPair list