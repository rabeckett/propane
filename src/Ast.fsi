module Ast

open Route

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
    | TemplateVar of Ident option * Ident
    | PrefixLiteral of int * int * int * int * int option
    | CommunityLiteral of int * int
    | Asn of int
    | IntLiteral of int
    | True
    | False

type ControlConstraints = (Ident * Expr list) list
type Definitions = Map<string, Position * Ident list * Expr>

type T = 
    {Input: string [];
     TopoInfo: Topology.TopoInfo;
     Defs: Definitions;
     CConstraints: ControlConstraints}

type PolicyPair = Predicate * Regex.REBuilder * Regex.T list

type CConstraint = 
    | CAggregate of Prefix * Set<string> * Set<string>
    | CCommunity of string * Prefix list * Set<string> * Set<string>
    | CMaxRoutes of int * Set<string> * Set<string>
    | CLongestPath of int * Set<string>

type PolInfo =
    {Ast: T;
     PredBuilder: PredicateBuilder;
     Policy: PolicyPair list;
     CConstraints: CConstraint list;
     OrigLocs: Map<Predicate, Set<string>>}

val build: T -> PolInfo