module Ast

type Position = 
    {SLine: int;
     SCol: int;
     ELine: int;
     ECol: int;}

type Ident = 
    {Pos: Position;
     Name: string}

type Expr =
    {Pos: Position;
     Node: Node}
and Node =
    | Ident of Ident * Expr list
    | BlockExpr of (Expr * Expr) list
    | LinkExpr of Expr * Expr
    | DiffExpr of Expr * Expr
    | ShrExpr of Expr * Expr
    | OrExpr of Expr * Expr
    | AndExpr of Expr * Expr
    | NotExpr of Expr
    | PrefixLiteral of uint32 * uint32 * uint32 * uint32 * uint32 option
    | CommunityLiteral of uint32 * uint32
    | Asn of uint32
    | IntLiteral of uint32
    | True
    | False

type ControlConstraints = (Ident * Expr list) list
type Definitions = Map<string, Position * Ident list * Expr>

type T = 
    {Input: string [];
     TopoInfo: Topology.TopoInfo;
     Defs: Definitions;
     CConstraints: ControlConstraints}

type PolicyPair = Predicate.T * Regex.REBuilder * Regex.T list

type CConstraint = 
    | CAggregate of Prefix.T * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32 * Set<string>

type PolInfo =
    {Ast: T;
     Policy: PolicyPair list;
     CConstraints: CConstraint list;
     OrigLocs: Map<Predicate.T, Set<string>>}

val build: T -> PolInfo