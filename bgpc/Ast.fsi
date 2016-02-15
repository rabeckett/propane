module Ast

/// AST file position information
type Position = 
    {SLine: int;
     SCol: int;
     ELine: int;
     ECol: int;}

type Expr =
    | Ident of Position * string * Expr list
    | BlockExpr of Position * (Expr * Expr) list
    | LinkExpr of Position * Expr * Expr
    | DiffExpr of Position * Expr * Expr
    | StarExpr of Position * Expr
    | ShrExpr of Position * Expr * Expr
    | OrExpr of Position * Expr * Expr
    | AndExpr of Position * Expr * Expr
    | NotExpr of Position * Expr
    | PrefixLiteral of Position * (uint32 * uint32 * uint32 * uint32 * uint32 option)
    | CommunityLiteral of Position * (uint32 * uint32) 
    | IntLiteral of Position * uint32
    | True of Position
    | False of Position

/// Individual control constraint 
type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32

type ControlConstraints = (string * Expr list) list
type Definitions = Map<string, string list * Expr>

/// Ast type with final definitions, control contraint, task definitions, and the final policy
type T = 
    {Input: string [];
     Defs: Definitions;
     CConstraints: ControlConstraints}

/// Final pairs of predicate, regular preferences after merging tasks
type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

val getPosition: Expr -> Position

/// Parse control constraint information w.r.t the topology
val getControlConstraints: T -> Topology.T -> CConstraint list

/// Build the final predicate, preference pairs by 
/// merging tasks and evaluating the main policy
val makePolicyPairs: T -> Topology.T -> PolicyPair list