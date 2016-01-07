module Ast

type Predicate =
    | True
    | False
    | Prefix of uint32 * uint32 * uint32 * uint32 * uint32 option 
    | Or of Predicate * Predicate
    | And of Predicate * Predicate

type Re = 
    | Empty
    | Concat of Re * Re
    | Union of Re * Re 
    | Inter of Re * Re 
    | Difference of Re * Re
    | Negate of Re
    | Star of Re
    | Ident of string * Re list

type Expr =
    | PredicateExpr of Predicate
    | LinkExpr of Re * Re
    | IntLiteral of uint32
    | IdentExpr of string

type PathConstraint = Predicate * (Re list)
type PathConstraints = PathConstraint list

type ConcretePathConstraint = Prefix.T list * Re list
type ConcretePathConstraints = ConcretePathConstraint list

type ControlConstraint = string * Expr list
type ControlConstraints = ControlConstraint list

type Task =
    {Name: string;
     PConstraints: PathConstraints}

type CConstraint = 
    | Aggregate of Prefix.T list * Set<string> * Set<string>

type PolicyPair = (Prefix.T list * Regex.REBuilder * Regex.T list)

type T = 
    {Defs: Map<string, Re>;
     CConstraints: ControlConstraints;
     Tasks: Task list;
     Policy: Re}

val getControlConstraints: T -> Topology.T -> CConstraint list

val makePolicyPairs: T -> Topology.T -> PolicyPair list