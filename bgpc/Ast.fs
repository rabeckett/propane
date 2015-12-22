module Ast

open Topology
open Common.Error

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


let rec buildRegex (reb: Regex.REBuilder) (r: Re) : Regex.LazyT =
    let checkParams id n args =
        let m = List.length args
        if m <> n then 
            error (sprintf "expected %d arguments for %s, but received %d" n id m) 
        let args = List.map (buildRegex reb) args
        let wf = List.map (Regex.singleLocations Set.empty) args
        if List.exists Option.isNone wf then 
            error (sprintf "parameter for %s must refer to locations only" id)
        else List.map (Option.get >> Set.toList) wf
    match r with
    | Empty -> reb.Empty 
    | Concat(x,y) -> reb.Concat [(buildRegex reb x); (buildRegex reb y)]
    | Inter(x,y) -> reb.Inter [(buildRegex reb x); (buildRegex reb y)]
    | Union(x,y) -> reb.Union [(buildRegex reb x); (buildRegex reb y)]
    | Negate x -> reb.Negate (buildRegex reb x)
    | Star x -> reb.Star (buildRegex reb x)
    | Ident(id, args) -> 
        match id with
        | "valleyfree" -> let locs = checkParams id args.Length args in reb.ValleyFree locs
        | "start" -> let locs = checkParams id 1 args in reb.StartsAtAny locs.Head
        | "end" -> let locs = checkParams id 1 args in reb.EndsAtAny locs.Head
        | "waypoint" -> let locs = checkParams id 1 args in reb.WaypointAny locs.Head
        | "avoid" -> let locs = checkParams id 1 args in reb.AvoidAny locs.Head
        | "internal" -> ignore (checkParams id 0 args); reb.Internal()
        | "external" -> ignore (checkParams id 0 args); reb.External()
        | "any" -> ignore (checkParams id 0 args); reb.Any()
        | "none" -> ignore (checkParams id 0 args); reb.Empty
        | "in" -> ignore (checkParams id 0 args); reb.Inside 
        | "out" -> ignore (checkParams id 0 args);  reb.Outside
        | l -> ignore (checkParams id 0 args); reb.Loc l

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

let makeDisjointPairs (sName: string) (pcs: PathConstraints) : ConcretePathConstraints =
    try 
        let mutable rollingPred = [Prefix.wholeRange]
        let mutable disjointPairs = []
        for (pred, res) in pcs do
            let ranges = Prefix.interAll (asRanges pred) rollingPred
            rollingPred <- Prefix.interAll rollingPred (Prefix.negateAll ranges)
            let options = List.map Prefix.prefixesOfRange ranges |> List.concat
            disjointPairs <- (options, res) :: disjointPairs
        if not (List.isEmpty rollingPred) then 
            let exPrefix = List.head (Prefix.prefixesOfRange (List.head rollingPred))
            let s = exPrefix.ToString()
            error (sprintf "Incomplete prefixes in scope (%s). An example of a prefix that is not matched: %s" sName s)
        List.rev disjointPairs
    with InvalidPrefixException p ->
        error (sprintf "Invalid prefix: %s" (p.ToString()))

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
        let s1 = rs1.ToString()
        let s2 = rs2.ToString()
        error (sprintf "Cannot combine multiple preferences in expansion of: (%s)%s(%s)" s1 opStr s2)
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
        parseError (sprintf "Empty constraint not allowed in main policy expression")
    | Concat(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OConcat
    | Union(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OUnion
    | Inter(x,y) -> combineConstraints (mergeScopes x disjoints) (mergeScopes y disjoints) OInter
    | Negate x -> 
        parseError (sprintf "Negation not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Star x -> 
        parseError (sprintf "Star operator not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Ident(x,res) -> 
        if res <> [] then
            parseError (sprintf "parameters given for identifier %s in main policy definition" x) 
        Map.find x disjoints

let makePolicyPairs (ast: T) (topo: Topology.T) : (Prefix.T list * Regex.REBuilder * Regex.T list) list =
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
    let addPair acc s = 
        Map.add s.Name (makeDisjointPairs s.Name s.PConstraints) acc
    let disjoints = List.fold addPair Map.empty ast.Scopes
    let allPCs = mergeScopes ast.Policy disjoints
    let mutable acc = []
    for (prefixes, res) in allPCs do 
        let reb = Regex.REBuilder(topo)
        let res = List.map (buildRegex reb) res 
        let res = List.map (fun r -> reb.Build r) res
        acc <- (prefixes, reb, res) :: acc
    List.rev acc