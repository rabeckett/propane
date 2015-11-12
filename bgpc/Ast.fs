module Ast
open Microsoft.FSharp.Collections

type Definition = string

type PathConstraint = Prefix.T * Regex.T * Regex.T

type ControlConstraint = 
    | Multipath
    | MaxRoutes of int
    | RouteAggregate of Prefix.T * Regex.T * Regex.T
    | CommunityTag of Prefix.T * Regex.T * Regex.T
    | Ownership of Prefix.T * Regex.T

type T =
    {Defs: Definition list;
     PConstraints: PathConstraint list;
     CConstraints: ControlConstraint list}