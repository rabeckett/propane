module Ast

open Topology
open Common.Error
open Common.Debug

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
    | Difference of Re * Re
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

type Task =
    {Name: string;
     PConstraints: PathConstraints;
     CConstraints: ControlConstraints}

type CConstraint = 
    | Aggregate of Prefix.T list * Set<string> * Set<string>

type T = 
    {Defs: Definition list;
     Tasks: Task list;
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
    | Difference(x,y) -> reb.Inter [(buildRegex reb x); reb.Negate (buildRegex reb y)]
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

let rec asRanges (p: Predicate) : Prefix.Pred = 
    match p with 
    | True -> Prefix.top
    | False -> Prefix.bot
    | And(a,b) -> Prefix.conj (asRanges a) (asRanges b)
    | Or(a,b) -> Prefix.disj (asRanges a) (asRanges b)
    | Not a -> Prefix.negation (asRanges a)
    | Prefix(a,b,c,d,bits) ->
        let adjustedBits = 
            match bits with
            | None -> 32u
            | Some x -> x
        let p = Prefix.prefix (a,b,c,d) adjustedBits
        if (a > 255u || b > 255u || c > 255u || d > 255u || adjustedBits > 32u) then
            raise (InvalidPrefixException p)
        Prefix.toPredicate [p]

let buildCConstraint (topo: Topology.T) cc =
    let reb = Regex.REBuilder(topo) 
    let (name, args) = cc
    match name with
    | "aggregate" ->
        match args with 
        | [a; b] ->
            match a with
            | PredicateExpr p -> 
                match b with
                | LinkExpr(x,y) ->
                    let locsX = Regex.singleLocations Set.empty (buildRegex reb x)
                    let locsY = Regex.singleLocations Set.empty (buildRegex reb y)
                    match locsX, locsY with 
                    | Some xs, Some ys -> Aggregate (Prefix.toPrefixes (asRanges p), xs, ys)
                    | _, _ -> error (sprintf "link expression parameter 2 to aggregate must denote single locations")
                | _ -> error (sprintf "parameter 2 to aggregate must be a link expression (e.g., in -> out)")
            | _ -> error (sprintf "parameter 1 to aggregate must be a prefix")
        | _ -> error (sprintf "aggregate constraint takes 2 arguments")
    | _ -> error (sprintf "unknown control constraint: %s" name)

let getControlConstraints (ast: T) reb = 
    ast.Tasks 
    |> List.map (fun s -> s.CConstraints)
    |> List.toSeq
    |> Seq.concat
    |> Seq.map (buildCConstraint reb)


(*
let makeDisjointPairs (sName: string) (pcs: ConcretePathConstraints) : ConcretePathConstraints =
    try 
        let mutable rollingPred = Prefix.top
        let mutable disjointPairs = []
        for (pred, res) in pcs do
            (* printfn "Rolling pred: %A" rollingPred *)
            let ranges = Prefix.conj (Prefix.toPredicate pred) rollingPred
            rollingPred <- Prefix.conj rollingPred (Prefix.negation ranges)
            let options = Prefix.toPrefixes ranges
            disjointPairs <- (options, res) :: disjointPairs
        if rollingPred <> Prefix.bot then 
            let exPrefix = List.head (Prefix.toPrefixes rollingPred)
            let s = exPrefix.ToString()
            error (sprintf "Incomplete prefixes in scope (%s). An example of a prefix that is not matched: %s" sName s)
        List.rev disjointPairs
    with InvalidPrefixException p ->
        error (sprintf "Invalid prefix: %s" (p.ToString())) *)

(* let makeCompactPairs (pcs: ConcretePathConstraints) : ConcretePathConstraints = 
    printfn "============================"
    for (p, _) in pcs do 
        printfn "%s" (string p)
    let mutable rollingPred = Prefix.bot
    let mutable newPCs = []
    for (prefix, res) in pcs do
        printfn "Existing predicate: %s" (prefix.ToString())
        printfn "Rolling predicate: %s" ((Prefix.toPrefixes rollingPred).ToString())
        let p = Prefix.toPredicate prefix
        let newP = Prefix.disj p rollingPred
        let newPrefix = Prefix.toPrefixes newP
        newPCs <- (newPrefix, res) :: newPCs
        rollingPred <- newP
        printfn "Rolling predicate (after): %s" ((Prefix.toPrefixes rollingPred).ToString())
    printfn "============================"
    List.rev newPCs *)

type BinOp = 
    | OConcat 
    | OInter 
    | OUnion 
    | ODifference

let applyOp r1 r2 op = 
    match op with
    | OConcat -> Re.Concat(r1,r2)
    | OInter -> Re.Inter(r1,r2)
    | OUnion -> Re.Union(r1,r2)
    | ODifference -> Re.Difference(r1,r2)

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
            | ODifference -> " \ "
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
    let mutable rollingPred = Prefix.bot
    for (ps, res) in pcs1 do 
        for (ps', res') in pcs2 do
            let rs = Prefix.toPredicate ps
            let rs' = Prefix.toPredicate ps'
            let comb = Prefix.conj rs rs'
            let asPref = Prefix.toPrefixes comb
            let notAbove = Prefix.conj comb (Prefix.negation rollingPred)
            if notAbove <> Prefix.bot then
                rollingPred <- Prefix.disj rollingPred comb
                let both = (asPref, combineRegexes res res' op)
                combined <- both :: combined
    List.rev combined

let checkPrefixes (sName: string) (pcs: ConcretePathConstraints) =
    try 
        let mutable rollingPred = Prefix.top
        for (pred, _) in pcs do
            let ranges = Prefix.conj (Prefix.toPredicate pred) rollingPred
            rollingPred <- Prefix.conj rollingPred (Prefix.negation ranges)
        if rollingPred <> Prefix.bot then 
            let exPrefix = List.head (Prefix.toPrefixes rollingPred)
            let s = exPrefix.ToString()
            error (sprintf "Incomplete prefixes in scope (%s). An example of a prefix that is not matched: %s" sName s)
    with InvalidPrefixException p ->
        error (sprintf "Invalid prefix: %s" (p.ToString()))

let rec mergeTasks (re: Re) disjoints : ConcretePathConstraints =
    match re with
    | Empty -> error (sprintf "Empty constraint not allowed in main policy expression")
    | Concat(x,y) -> combineConstraints (mergeTasks x disjoints) (mergeTasks y disjoints) OConcat
    | Union(x,y) -> combineConstraints (mergeTasks x disjoints) (mergeTasks y disjoints) OUnion
    | Inter(x,y) -> combineConstraints (mergeTasks x disjoints) (mergeTasks y disjoints) OInter
    | Difference(x,y) -> combineConstraints (mergeTasks x disjoints) (mergeTasks y disjoints) ODifference
    | Negate x -> error (sprintf "Negation not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Star x -> error (sprintf "Star operator not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Ident(x,res) -> 
        if res <> [] then
            error (sprintf "parameters given for identifier %s in main policy definition" x) 
        Map.find x disjoints

let addPair acc s =
    let cconstrs = List.map (fun (p,r) -> (Prefix.toPrefixes (asRanges p),r)) s.PConstraints
    checkPrefixes s.Name cconstrs
    Map.add s.Name cconstrs acc

let makePolicyPairs (ast: T) (topo: Topology.T) : (Prefix.T list * Regex.REBuilder * Regex.T list) list =
    let names = List.map (fun s -> s.Name) ast.Tasks
    let unqNames = Set.ofList names
    if unqNames.Count <> names.Length then
        let dups =
            names 
            |> Seq.ofList
            |> Seq.countBy id
            |> Seq.filter (fun (_,i) -> i > 1)
            |> Seq.map fst
        error (sprintf "duplicate named policies: %s" (dups.ToString()))
    let disjoints = List.fold addPair Map.empty ast.Tasks
    let allPCs = mergeTasks ast.Policy disjoints
    let mutable acc = []
    for (prefixes, res) in allPCs do 
        let reb = Regex.REBuilder(topo)
        let res = List.map (buildRegex reb) res 
        let res = List.map reb.Build res
        acc <- (prefixes, reb, res) :: acc
    List.rev acc