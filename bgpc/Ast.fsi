module Ast

/// AST expression
type Expr =
    | Ident of string * Expr list
    | BlockExpr of (Expr * Expr) list
    | LinkExpr of Expr * Expr
    | DiffExpr of Expr * Expr
    | StarExpr of Expr
    | ShrExpr of Expr * Expr
    | OrExpr of Expr * Expr
    | AndExpr of Expr * Expr
    | NotExpr of Expr
    | PrefixLiteral of uint32 * uint32 * uint32 * uint32 * uint32 option
    | CommunityLiteral of uint32 * uint32 
    | IntLiteral of uint32
    | True
    | False

/// Individual control constraint 
type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32

type PathConstraints = (Expr * Expr) list
type ControlConstraints = (string * Expr list) list

/// Ast type with final definitions, control contraint, task definitions, and the final policy
type T = 
    {Defs: Map<string, Expr>;
     CConstraints: ControlConstraints}

/// Final pairs of predicate, regular preferences after merging tasks
type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

/// Parse control constraint information w.r.t the topology
val getControlConstraints: T -> Topology.T -> CConstraint list

/// Build the final predicate, preference pairs by 
/// merging tasks and evaluating the main policy
val makePolicyPairs: T -> Topology.T -> PolicyPair list