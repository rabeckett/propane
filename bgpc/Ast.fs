module Ast

open Topology
open Prefix

(* Inside/Outside, Loc are part of single identifier definition *)
type Re = 
    | Empty
    | Concat of Re * Re
    | Union of Re * Re 
    | Inter of Re * Re 
    | Negate of Re 
    | Star of Re
    | Ident of string * Re list

type Definition = string

type Predicate =
    | True
    | False
    | Prefix of uint32 * uint32 * uint32 * uint32 * uint32 option 
    | Or of Predicate * Predicate
    | And of Predicate * Predicate
    | Not of Predicate

type Expr =
    | PredicateExpr of Predicate
    | LinkExpr of Re * Re
    | IntLiteral of uint32
    | IdentExpr of string

type PathConstraint = Predicate * (Re list)

type ControlConstraint = string * Expr list

(*
type ControlConstraint = 
    | Multipath
    | MaxRoutes of int
    | RouteAggregate of Predicate * Re * Re
    | CommunityTag of Predicate * Re * Re
    | Ownership of Predicate * Re *)

type Scope =
    {Name: string;
     Defs: Definition list;
     PConstraints: PathConstraint list;
     CConstraints: ControlConstraint list}

type T = Scope list

exception InvalidPrefixException of Prefix.T


let rec buildRegex (reb: Regex.REBuilder) (r: Re) : Regex.T = 
    match r with
    | Empty -> reb.Empty 
    | Concat(x,y) -> reb.Concat (buildRegex reb x) (buildRegex reb y)
    | Inter(x,y) -> reb.Inter (buildRegex reb x) (buildRegex reb y)
    | Union(x,y) -> reb.Union (buildRegex reb x) (buildRegex reb y)
    | Negate x -> reb.Negate (buildRegex reb x)
    | Star x -> reb.Star (buildRegex reb x)
    | Ident(id, args) -> 
        match id, args.Length with
        | "valleyfree", n when n > 0 -> 
            let args = List.map (buildRegex reb) args
            let wf = List.map (Regex.singleLocations reb.Alphabet) args
            if List.exists Option.isNone wf then 
                failwith "[Error]: jam"
            else
                let locs = List.map (Option.get >> Set.toList) wf
                reb.ValleyFree locs
        | "start", 1 -> 
            let hd = buildRegex reb (List.head args)
            match Regex.singleLocations (reb.Alphabet) hd with
            | None -> failwith "[Error]: Foo"
            | Some ls -> reb.StartsAtAny(Set.toList ls)
        | "end", 1 ->
            let hd = buildRegex reb (List.head args)
            match Regex.singleLocations (reb.Alphabet) hd with
            | None -> failwith "[Error]: Bar"
            | Some ls ->
                reb.EndsAtAny(Set.toList ls)
        | "waypoint", 1 ->
            let hd = buildRegex reb (List.head args)
            match Regex.singleLocations (reb.Alphabet) hd with
            | None -> failwith "[Error]: Baz"
            | Some ls -> reb.WaypointAny(Set.toList ls)
        | "avoid", 1 ->
            let hd = buildRegex reb (List.head args)
            match Regex.singleLocations (reb.Alphabet) hd with
            | None -> failwith "[Error]: Fab"
            | Some ls -> reb.AvoidAny(Set.toList ls)
        | "internal", 0 -> reb.Internal()
        | "external", 0 -> reb.External()
        | "any", 0 -> reb.Any()
        | "in", 0 -> reb.Inside 
        | "out", 0 -> reb.Outside
        | l, 0 -> reb.Loc l
        | _, _ -> failwith ("[Error]: Unknown definition: " + id)


let rec asRanges (p: Predicate) : Prefix.Ranges = 
    match p with 
    | True -> [wholeRange]
    | False -> [] 
    | And(a,b) -> interAll (asRanges a) (asRanges b)
    | Or(a,b) -> unionAll (asRanges a) (asRanges b)
    | Not a -> negateAll (asRanges a)
    | Prefix(a,b,c,d,bits) ->
        let adjustedBits = 
            match bits with
            | None -> 32u
            | Some x -> x
        let p = Prefix.T(a,b,c,d,adjustedBits)
        if (a > 255u || b > 255u || c > 255u || d > 255u || adjustedBits > 32u) then
            raise (InvalidPrefixException p)
        [rangeOfPrefix p]