module Ast

open Topology
open Common.Error

(* Inside/Outside, Loc are part of single identifier definition *)

type Predicate =
    | True
    | False
    | Prefix of uint32 * uint32 * uint32 * uint32 * uint32 option 
    | Or of Predicate * Predicate
    | And of Predicate * Predicate
    | Not of Predicate

type Re = 
    | Empty
    | Concat of Re * Re
    | Union of Re * Re 
    | Inter of Re * Re 
    | Negate of Re 
    | Star of Re
    | Ident of string * Re list

type Definition = string

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

type Scope =
    {Name: string;
     PConstraints: PathConstraints;
     CConstraints: ControlConstraints}

type T = 
    {Defs: Definition list;
     Scopes: Scope list;
     Policy: Re}

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
        | _, _ ->
            error (sprintf "Unknown definition %s" id)

let rec asRanges (p: Predicate) : Prefix.Ranges = 
    match p with 
    | True -> [Prefix.wholeRange]
    | False -> [] 
    | And(a,b) -> Prefix.interAll (asRanges a) (asRanges b)
    | Or(a,b) -> Prefix.unionAll (asRanges a) (asRanges b)
    | Not a -> Prefix.negateAll (asRanges a)
    | Prefix(a,b,c,d,bits) ->
        let adjustedBits = 
            match bits with
            | None -> 32u
            | Some x -> x
        let p = Prefix.T(a,b,c,d,adjustedBits)
        if (a > 255u || b > 255u || c > 255u || d > 255u || adjustedBits > 32u) then
            raise (InvalidPrefixException p)
        [Prefix.rangeOfPrefix p]

let makeDisjointPairs (sName: string) (pcs: PathConstraints) reb : ConcretePathConstraints =
    try 
        let mutable rollingPred = [Prefix.wholeRange]
        let mutable disjointPairs = []
        for (pred, res) in pcs do
            let ranges = Prefix.interAll (asRanges pred) rollingPred
            rollingPred <- Prefix.interAll rollingPred (Prefix.negateAll ranges)
            let options = List.map Prefix.prefixesOfRange ranges |> List.concat
            disjointPairs <- (options, res) :: disjointPairs
        if not (List.isEmpty rollingPred) then 
            let p = (List.head (Prefix.prefixesOfRange (List.head rollingPred))).ToString()
            error (sprintf "Incomplete prefixes in scope (%s). An example of a prefix that is not matched: %s" sName p)
        List.rev disjointPairs
    with InvalidPrefixException p ->
        let s = sprintf "Invalid prefix: %s" (p.ToString())
        error s

type BinOp = OConcat | OInter | OUnion

let applyOp r1 r2 op = 
    match op with
    | OConcat -> Re.Concat(r1,r2)
    | OInter -> Re.Inter(r1,r2)
    | OUnion -> Re.Union(r1,r2)


let combineRegexes (rs1: Re list) (rs2: Re list) (op: BinOp) : Re list =
    let len1 = List.length rs1 
    let len2 = List.length rs2
    assert (len1 > 0)
    assert (len2 > 0)
    if len1 > 1 && len2 > 1 then 
        let opStr = 
            match op with
            | OConcat -> ";"
            | OInter -> " and "
            | OUnion -> " + "
        printfn 
            "\n[Error]: Cannot combine multiple preferences in expansion of: (%s)%s(%s)" 
            (rs1.ToString()) 
            opStr 
            (rs2.ToString())
        exit 0
    if len1 >= len2 then 
        let r2 = rs2.Head
        List.map (fun r1 -> applyOp r1 r2 op) rs1
    else
        let r1 = rs1.Head
        List.map (fun r2 -> applyOp r1 r2 op) rs2

let combineConstraints (pcs1: ConcretePathConstraints) (pcs2: ConcretePathConstraints) (op: BinOp) =
    let mutable combined = []
    for (ps, res) in pcs1 do 
        for (ps', res') in pcs2 do 
            let rs = Prefix.rangeOfPrefixes ps
            let rs' = Prefix.rangeOfPrefixes ps'
            let conj = Prefix.interAll rs rs'
            let asPref = Prefix.prefixesOfRanges conj
            let both = (asPref, combineRegexes res res' op)
            combined <- both :: combined
    combined
    |> List.filter (fun (x,_) -> not (List.isEmpty x))
    |> List.rev

let rec mergeScopes (re: Re) disjoints : ConcretePathConstraints =
    match re with
    | Empty -> 
        error (sprintf "Empty constraint not allowed in main policy expression")
    | Concat(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OConcat
    | Union(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OUnion
    | Inter(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OInter
    | Negate x ->
        error (sprintf "Negation not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Star x -> 
        error (sprintf "Star operator not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Ident(x,res) -> 
        if not (List.isEmpty res) then
            error (sprintf "parameters given for identifier %s in main policy definition" x) 
        Map.find x disjoints

let makePolicyPairs (ast: T) reb : (Prefix.T list * Regex.T list) list =
    let names = List.map (fun s -> s.Name) ast.Scopes
    let unqNames = Set.ofList names
    if unqNames.Count <> names.Length then
        let dups =
            names 
            |> Seq.ofList
            |> Seq.countBy id
            |> Seq.filter (fun (_,i) -> i > 1)
            |> Seq.map fst
        error (sprintf "duplicate named policies: %s" (dups.ToString()))
    let disjoints = List.fold (fun acc s -> Map.add s.Name (makeDisjointPairs s.Name s.PConstraints reb) acc) Map.empty ast.Scopes
    let allPCs = mergeScopes ast.Policy disjoints
    List.map (fun (prefixes, res) -> (prefixes, List.map (buildRegex reb) res)) allPCs