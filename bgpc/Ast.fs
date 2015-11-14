module Ast
open Topology

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

type PathConstraint = Prefix.T * (Re list)

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

let rec buildRegex (reb: Regex.REBuilder) (r: Re) : Regex.T = 
    match r with 
    | Inside -> reb.Inside
    | Outside -> reb.Outside 
    | Loc l -> reb.Loc l 
    | Empty -> reb.Empty 
    | Concat(x,y) -> reb.Concat (buildRegex reb x) (buildRegex reb y)
    | Inter(x,y) -> reb.Inter (buildRegex reb x) (buildRegex reb y)
    | Union(x,y) -> reb.Union (buildRegex reb x) (buildRegex reb y)
    | Negate x -> reb.Negate (buildRegex reb x)
    | Star x -> reb.Star (buildRegex reb x)

