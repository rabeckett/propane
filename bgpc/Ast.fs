module Ast

open Topology
open Common.Error
open Common.Debug

type Predicate =
    | True
    | False
    | Prefix of uint32 * uint32 * uint32 * uint32 * uint32 option
    | Community of uint32 * uint32 
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
    | Shr of Re * Re
    | Ident of string * Re list

type Expr =
    | PredicateExpr of Predicate
    | LinkExpr of Re * Re
    | IntLiteral of uint32
    | IdentExpr of string

type PathConstraint = Predicate * Re
type PathConstraints = PathConstraint list

type ConcretePathConstraint = Predicate.T * Re list
type ConcretePathConstraints = ConcretePathConstraint list

type ControlConstraint = string * Expr list
type ControlConstraints = ControlConstraint list

type Task =
    {Name: string;
     PConstraints: PathConstraints}

type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of Prefix.T list * Set<string> * Set<string> * string
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32

type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

type Def = 
    | DRegex of Re
    | DExpr of Expr
    | DBuiltin

type T = 
    {Defs: Map<string, Def>;
     CConstraints: ControlConstraints;
     Tasks: Task list;
     Policy: Re}

exception InvalidPrefixException of Prefix.T

let rec pushPrefsToTop (r: Re) : Re list = 
    match r with 
    | Empty -> [Empty]
    | Concat (x,y) -> merge r (x,y) (fun a b -> Concat(a,b))
    | Inter (x,y) -> merge r (x,y) (fun a b -> Inter(a,b))
    | Union (x,y) -> merge r (x,y) (fun a b -> Union(a,b))
    | Difference (x,y) -> merge r (x,y) (fun a b -> Difference(a,b))
    | Star x -> mergeSingle r x "star"
    | Negate x -> mergeSingle r x "negation"
    | Shr (x,y) -> (pushPrefsToTop x) @ (pushPrefsToTop y)
    | Ident _ -> [r] (* TODO: invariant - no prefs in expansion, remove from REBuilder code *)
and merge r (x,y) f =
    let xs = pushPrefsToTop x 
    let ys = pushPrefsToTop y
    match xs, ys with
    | [a], _ -> List.map (fun b -> f a b) ys
    | _, [b] -> List.map (fun a -> f a b) xs
    | a::_, b::_ -> 
        let sx, sy, sr = string x, string y, string r 
        error (sprintf "invalid use of preferences in regex, cannot merge: %s and %s in %s" sx sy sr)
    | _, _ -> failwith "impossible"
and mergeSingle r x op =
    let xs = pushPrefsToTop x
    if xs.Length > 1 then
        error (sprintf "invalid use of preferences in regex, cannot nest under %s operator: %s" op (string r))
    else xs 

let builtInRes = 
    Set.ofList 
        ["start"; "end"; "valleyfree"; "waypoint"; "avoid"; 
         "internal"; "external"; "any"; "none"; "in"; "out"]

let builtInConstraints = 
    Set.ofList ["aggregate"; "tag"; "maxroutes"; "longest_path"]

let builtIns = Set.union builtInRes builtInConstraints

let validateDefinitions defs = 
    Map.iter (fun l _ -> 
        if builtIns.Contains l then
            error (sprintf "invalid redefinition of %s" l)
        else ()) defs

let rec lookupDefinition defs l = 
    match Map.tryFind l defs with
    | None -> None
    | Some x -> 
        match x with 
        | DRegex (Ident(s,[]))
        | DExpr (IdentExpr s) ->
            match lookupDefinition defs s with
            | None -> Some x
            | Some y -> Some y
        | _ -> Some x

let rec buildRegex (ast: T) (reb: Regex.REBuilder) (r: Re) : Regex.LazyT =
    let checkParams id n args =
        let m = List.length args
        if m <> n then 
            error (sprintf "expected %d arguments for %s, but received %d" n id m) 
        let args = List.map (buildRegex ast reb) args
        let wf = List.map (Regex.singleLocations (reb.Topo())) args
        if List.exists Option.isNone wf then 
            error (sprintf "parameter for %s must refer to locations only" id)
        else List.map (Option.get >> Set.toList) wf
    match r with
    | Empty -> reb.Empty 
    | Concat(x,y) -> reb.Concat [(buildRegex ast reb x); (buildRegex ast reb y)]
    | Inter(x,y) -> reb.Inter [(buildRegex ast reb x); (buildRegex ast reb y)]
    | Union(x,y) -> reb.Union [(buildRegex ast reb x); (buildRegex ast reb y)]
    | Difference(x,y) -> reb.Inter [(buildRegex ast reb x); reb.Negate (buildRegex ast reb y)]
    | Negate x -> reb.Negate (buildRegex ast reb x)
    | Star x -> reb.Star (buildRegex ast reb x)
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
        | l -> 
            ignore (checkParams id 0 args)
            (* TODO: check for recursive definition *)
            match lookupDefinition ast.Defs l with 
            | Some (DRegex r) -> buildRegex ast reb r
            | Some (DExpr e) -> 
                error (sprintf "expected regex, but got %s in expansion of definition %s for regex %s" (string e) l (string r))
            | None -> reb.Loc l 
            | Some (DBuiltin) -> failwith "impossible"
    | Shr _ -> failwith "unreachable"

let validatePrefix (a,b,c,d,adjBits) =
    let p = Prefix.prefix (a,b,c,d) adjBits
    if (a > 255u || b > 255u || c > 255u || d > 255u || adjBits > 32u) then
        raise (InvalidPrefixException p)

let rec toPredicate (p: Predicate) : Predicate.T = 
    match p with 
    | True -> Predicate.top
    | False -> Predicate.bot
    | And(a,b) -> Predicate.conj (toPredicate a) (toPredicate b)
    | Or(a,b) -> Predicate.disj (toPredicate a) (toPredicate b)
    | Not a -> Predicate.negate (toPredicate a)
    | Prefix(a,b,c,d,bits) ->
        let adjBits = 
            match bits with
            | None -> 32u
            | Some x -> x
        validatePrefix (a,b,c,d,adjBits)
        Predicate.prefix (a,b,c,d) adjBits
    | Community(x,y) ->  Predicate.community (string x + ":" + string y)

let rec toPrefixes (p: Predicate) : Prefix.Pred = 
    match p with 
    | True -> Prefix.top
    | False -> Prefix.bot 
    | And(a,b) -> Prefix.conj (toPrefixes a) (toPrefixes b)
    | Or(a,b) -> Prefix.disj (toPrefixes a) (toPrefixes b)
    | Not a -> Prefix.negation (toPrefixes a)
    | Prefix(a,b,c,d,bits) ->
        let adjBits = 
            match bits with
            | None -> 32u
            | Some x -> x
        validatePrefix (a,b,c,d,adjBits)
        Prefix.toPredicate [Prefix.prefix (a,b,c,d) adjBits]
    | Community(x,y) -> 
        parseError (sprintf "Invalid Community predicate: (%d: %d) " x y)
 
type Typ = 
    | PredicateType
    | CommunityType
    | LinkTyp
    | IntTyp

    override this.ToString() = 
        match this with
        | PredicateType -> "Predicate"
        | CommunityType -> "Community"
        | LinkTyp -> "Links"
        | IntTyp -> "Int"

let paramInfo = 
    Map.ofList [
        ("aggregate", (2, [PredicateType; LinkTyp]));
        ("tag", (3, [CommunityType; PredicateType; LinkTyp]));
        ("maxroutes", (2, [IntTyp; LinkTyp]));
        ("longest_path", (1, [IntTyp]));
    ]

let rec checkArgs ast name args = 
    match Map.tryFind name paramInfo with
    | None -> error (sprintf "unrecognized constraint: %s" name)
    | Some (n, typs) -> 
        let len = List.length args
        if len <> n then 
            error (sprintf "invalid number of parameters to: %s. Expected: %d, but received %d" name n len)
        let subst_args = 
            List.mapi (fun i arg -> 
                match arg with
                | IdentExpr s -> 
                    match lookupDefinition ast.Defs s with
                    | Some (DExpr e) -> e
                    | Some (DRegex _)
                    | Some DBuiltin ->
                        error (sprintf "parameter %d, (%s) to constraint %s is a regular expression" i s name)
                    | None -> error (sprintf "parameter %d, (%s) to constraint %s is undefined" i s name)
                | _ -> arg ) args
        let mutable i = 0
        for (x,y) in List.zip args typs do 
            i <- i + 1
            match x,y with
            | PredicateExpr _, PredicateType -> ()
            | PredicateExpr (Community _), CommunityType -> ()
            | LinkExpr(x,y), LinkTyp -> ()
            | IntLiteral _, IntTyp -> ()
            | IdentExpr _, _ -> failwith "impossible"
            | _, _ -> error (sprintf "expected parameter %d of constraint (%s) to be of type: %s" i name (string y))

let getPredicate x = 
    match x with 
    | PredicateExpr p -> p
    | _ -> failwith "unreachable"

let getLinks x = 
    match x with 
    | LinkExpr(x,y) -> (x,y)
    | _ -> failwith "unreachable"

let getInt x = 
    match x with 
    | IntLiteral i -> i 
    | _ -> failwith "unreachable"

let getComm x = 
    match x with 
    | Community(a,b) -> (a,b)
    | _ -> failwith "unreachable"

let buildCConstraint ast (topo: Topology.T) cc =
    let reb = Regex.REBuilder(topo) 
    let (name, args) = cc
    checkArgs ast name args

    let getLinkLocations (x,y) =
        let locsX = Regex.singleLocations (reb.Topo()) (buildRegex ast reb x)
        let locsY = Regex.singleLocations (reb.Topo()) (buildRegex ast reb y)
        match locsX, locsY with 
        | Some xs, Some ys -> (xs,ys)
        | _, _ -> error (sprintf "link expression must denote single locations in parameter to: %s" name)

    match name with
    | "aggregate" ->
        let p = getPredicate (List.head args)
        let (x,y) = getLinks (List.nth args 1)
        let (xs,ys) = getLinkLocations (x,y)
        CAggregate (Prefix.toPrefixes (toPrefixes p), xs, ys)
    | "tag" -> 
        let (a,b) = getComm (getPredicate (List.head args))
        let p = getPredicate (List.nth args 1)
        let (x,y) = getLinks (List.nth args 2)
        let (xs,ys) = getLinkLocations (x,y)
        let str = (string a) + ":" + (string b)
        CCommunity (Prefix.toPrefixes (toPrefixes p), xs, ys, str)
    | "maxroutes" ->
        let i = getInt (List.head args)
        let (x,y) = getLinks (List.nth args 1)
        let (xs,ys) = getLinkLocations (x,y)
        CMaxRoutes (i,xs,ys)
    | "longest_path" -> 
        let i = getInt (List.head args)
        CLongestPath i
    | _ -> failwith "unreachable"

let getControlConstraints (ast: T) topo = 
    List.map (buildCConstraint ast topo) ast.CConstraints

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
    let mutable rollingPred = Predicate.bot
    for (ps, res) in pcs1 do 
        for (ps', res') in pcs2 do
            let comb = Predicate.conj ps ps'
            let notAbove = Predicate.conj comb (Predicate.negate rollingPred)
            if notAbove <> Predicate.bot then
                rollingPred <- Predicate.disj rollingPred comb
                let both = (comb, combineRegexes res res' op)
                combined <- both :: combined
    List.rev combined

let checkPredicates (sName: string) (pcs: ConcretePathConstraints) =
    try 
        let mutable rollingPred = Predicate.top
        for (pred, _) in pcs do
            let p = Predicate.conj pred rollingPred
            rollingPred <- Predicate.conj rollingPred (Predicate.negate p)
        if rollingPred <> Predicate.bot then
            let s = Predicate.example rollingPred
            error (sprintf "Incomplete prefixes in scope (%s). An example of a prefix that is not matched: %s" sName s)
    with InvalidPrefixException p ->
        error (sprintf "Invalid prefix: %s" (p.ToString()))

let rec mergeTasks ast (re: Re) disjoints : ConcretePathConstraints =
    match re with
    | Empty -> error (sprintf "Empty constraint not allowed in main policy expression")
    | Concat(x,y) -> combineConstraints (mergeTasks ast x disjoints) (mergeTasks ast y disjoints) OConcat
    | Union(x,y) -> combineConstraints (mergeTasks ast x disjoints) (mergeTasks ast y disjoints) OUnion
    | Inter(x,y) -> combineConstraints (mergeTasks ast x disjoints) (mergeTasks ast y disjoints) OInter
    | Difference(x,y) -> combineConstraints (mergeTasks ast x disjoints) (mergeTasks ast y disjoints) ODifference
    | Negate x -> error (sprintf "Negation not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Star x -> error (sprintf "Star operator not allowed in main policy definition, in expression: %s" (x.ToString()))
    | Ident(x,[]) -> 
        match Map.tryFind x disjoints with 
        | Some d -> d
        | None -> 
            match lookupDefinition ast.Defs x, builtInRes.Contains x with 
            | Some _, _ | _, true -> [(Predicate.top, [re])]
            | None, false -> error (sprintf "undefined identifier %s in main policy expression" x)
    | Ident(x,args) -> [(Predicate.top, [re])]
    | Shr _ -> failwith "unreachable"

let addPair acc s =
    let cconstrs = List.map (fun (p,r) -> (toPredicate p, pushPrefsToTop r)) s.PConstraints
    checkPredicates s.Name cconstrs
    Map.add s.Name cconstrs acc

let makePolicyPairs (ast: T) (topo: Topology.T) : PolicyPair list =
    validateDefinitions ast.Defs
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
    let allPCs = mergeTasks ast ast.Policy disjoints
    let mutable acc = []
    for (pred, res) in allPCs do 
        let reb = Regex.REBuilder(topo)
        let res = List.map (buildRegex ast reb) res 
        let res = List.map reb.Build res
        acc <- (pred, reb, res) :: acc
    List.rev acc