module Ast
open Microsoft.FSharp.Collections


type Re = 
    | Empty
    | Inside
    | Outside
    | Loc of string 
    | Concat of Re * Re
    | Union of Re * Re 
    | Inter of Re * Re 
    | Negate of Re 
    | Star of Re

type Definition = string

type PathConstraint = Path of Prefix.T * Re list

type ControlConstraint = 
    | Multipath
    | MaxRoutes of int
    | RouteAggregate of Prefix.T * Re * Re
    | CommunityTag of Prefix.T * Re * Re
    | Ownership of Prefix.T * Re

type Scope =
    {Name: string;
     Defs: Definition list;
     PConstraints: PathConstraint list;
     CConstraints: ControlConstraint list}

type T = Scope list
