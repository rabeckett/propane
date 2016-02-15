module Ast

open Topology
open Common.Error

type Position = 
    {SLine: int;
     SCol: int;
     ELine: int;
     ECol: int;}

type Expr =
    | Ident of Position * string * Expr list
    | BlockExpr of Position * (Expr * Expr) list
    | LinkExpr of Position * Expr * Expr
    | DiffExpr of Position * Expr * Expr
    | StarExpr of Position * Expr
    | ShrExpr of Position * Expr * Expr
    | OrExpr of Position * Expr * Expr
    | AndExpr of Position * Expr * Expr
    | NotExpr of Position * Expr
    | PrefixLiteral of Position * (uint32 * uint32 * uint32 * uint32 * uint32 option)
    | CommunityLiteral of Position * (uint32 * uint32) 
    | IntLiteral of Position * uint32
    | True of Position
    | False of Position

type Type = 
    | LinkType
    | RegexType
    | PredicateType
    | IntType
    | ControlType
    | BlockType

    override this.ToString() = 
        match this with
        | LinkType -> "Links"
        | RegexType -> "Regex"
        | PredicateType -> "Predicate"
        | IntType -> "Int"
        | ControlType -> "Control Constraint"
        | BlockType -> "Block"

type Value = 
    | PrefixValue
    | CommunityValue
    | LinkValue
    | IntValue

    override this.ToString() = 
        match this with
        | PrefixValue -> "Predicate"
        | CommunityValue -> "Community"
        | LinkValue -> "Links"
        | IntValue -> "Int"

type PathConstraints = (Expr * Expr) list
type ControlConstraints = (string * Expr list) list

type Task =
    {Name: string;
     PConstraints: PathConstraints}

type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32

type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

type T = 
    {Defs: Map<string, string list * Expr>;
     CConstraints: ControlConstraints}


(* Built-in control constraints and path constraints.
   May add a way for the user to define new constraints. *)

let paramInfo = 
    Map.ofList 
        [("aggregate", (2, [PrefixValue; LinkValue]));
         ("tag", (3, [CommunityValue; PrefixValue; LinkValue]));
         ("maxroutes", (2, [IntValue; LinkValue]));
         ("longest_path", (1, [IntValue])) ]

let builtInRes = 
    Set.ofList 
        ["start"; "end"; "enter"; 
         "exit"; "valleyfree"; "always"; 
         "through"; "avoid"; "internal"; 
         "external"; "any"; "drop"; "in"; "out"]

let builtInConstraints = 
    Set.ofList 
        ["aggregate"; "tag"; 
         "maxroutes"; "longest_path"]

let builtIns = Set.union builtInRes builtInConstraints

let dummyPos = 
    {SLine = -1;
     SCol = -1;
     ELine = -1;
     ECol = -1}

let format (e: Expr) : string =
    ""

(* Helper functions *)

let getPosition (e: Expr) = 
    match e with
    | LinkExpr (p, _, _)  
    | DiffExpr (p, _, _)  
    | ShrExpr (p, _, _) 
    | OrExpr (p, _, _) 
    | AndExpr (p, _, _)
    | StarExpr (p,_)
    | BlockExpr (p,_)
    | NotExpr (p,_)
    | True p | False p | PrefixLiteral (p,_) | CommunityLiteral (p,_) | IntLiteral (p,_)
    | Ident (p, _, _) -> p

let rec iter f (e: Expr) =
    f e
    match e with
    | BlockExpr (_,es) -> List.iter (fun (e1,e2) -> iter f e1; iter f e2) es
    | LinkExpr (_, e1, e2)  
    | DiffExpr (_, e1, e2)  
    | ShrExpr (_, e1, e2) 
    | OrExpr (_, e1, e2) 
    | AndExpr (_, e1, e2) -> iter f e1; iter f e2
    | StarExpr (_,e1)
    | NotExpr (_,e1) -> iter f e1
    | True _ | False _ | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ -> ()
    | Ident (_, id, args) -> List.iter f args

(* Compiler warnings for common mistakes *)

let getUsed seen e = 
    match e with 
    | Ident(_, id, _) -> 
        seen := Set.add id !seen
    | _ -> ()

let warnUnusedDefs ast e =
    let used = ref (Set.singleton "main")
    iter (getUsed used) e
    Map.iter (fun _ (_,e) -> iter (getUsed used) e) ast.Defs
    let defs = Map.fold (fun acc k _ -> Set.add k acc) Set.empty ast.Defs
    let unused = Set.difference defs !used
    if Set.count unused > 0 then
        let msg = Common.Set.joinBy "," unused
        warning (sprintf "Unused definition(s): %s" msg)

let warnUnusedParams (ast: T) = 
    Map.iter (fun id (ps,e) ->
        let seen = ref (Set.empty)
        iter (getUsed seen) e
        let unused = Set.difference (Set.ofList ps) !seen
        if Set.count unused > 0 then 
            let msg = Common.Set.joinBy "," unused
            warning (sprintf "Unused parameter(s): %s in definition %s" msg id)
        )ast.Defs

let warnUnused (ast: T) (e: Expr) : unit = 
    warnUnusedDefs ast e
    warnUnusedParams ast

(* Recursively expand all definitions in 
   an expression. Keep track of seen expansions
   to prevent recursive definitions *)

let substitute (defs: Map<string, string list * Expr>) (e: Expr) : Expr = 
    let rec aux defs seen e =
        match e with
        | BlockExpr (p,es) -> BlockExpr (p, List.map (fun (e1,e2) -> (aux defs seen e1, aux defs seen e2)) es)
        | LinkExpr (p, e1, e2) -> LinkExpr (p, aux defs seen e1, aux defs seen e2)
        | DiffExpr (p, e1, e2) -> DiffExpr (p, aux defs seen e1, aux defs seen e2)
        | ShrExpr (p, e1, e2) -> ShrExpr (p, aux defs seen e1, aux defs seen e2)
        | OrExpr (p, e1, e2) -> OrExpr (p, aux defs seen e1, aux defs seen e2)
        | AndExpr (p, e1, e2) -> AndExpr (p, aux defs seen e1, aux defs seen e2)
        | StarExpr (p, e1) -> StarExpr (p, aux defs seen e1)
        | NotExpr (p, e1) -> NotExpr (p, aux defs seen e1)
        | True _ | False _ | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ -> e
        | Ident (_, id, []) ->
            if Set.contains id seen then error (sprintf "Recursive definition of %s" id)
            match Map.tryFind id defs with
            | None -> e
            | Some (_,e1) -> aux defs (Set.add id seen) e1
        | Ident (p, id,es) -> 
            // substitute for args
            if Set.contains id seen then error (sprintf "Recursive definition of %s" id)
            match Map.tryFind id defs with
            | None -> Ident(p, id, List.map (aux defs seen) es)
            | Some (ids,e1) -> 
                let required = List.length ids 
                let provided = List.length es
                if required <> provided then 
                    error (sprintf "\nInvalid number of parameters to function %s \nRequired %d parameters, but provided %d" id required provided)
                let defs' =
                    List.zip ids es
                    |> List.fold (fun acc (id,e) -> Map.add id ([], e) acc) defs
                aux defs' (Set.add id seen) e1
    aux defs Set.empty e

(* Test well-formedness of an expression. 
   Ensures expressions are of the proper type, 
   that prefixes and links are valid, and 
   that blocks are never nested inside each other. *)

let inline adjustBits bits = 
    match bits with 
    | None -> 32u
    | Some b -> b

let wellFormedPrefix (a,b,c,d,bits) =
    let bits = adjustBits bits
    let p = Prefix.prefix (a,b,c,d) bits
    if (a > 255u || b > 255u || c > 255u || d > 255u || (bits > 32u)) then
        error (sprintf "Invalid prefix %s" (string p))

let wellFormed (e: Expr) : Type =
    let rec aux block e =
        match e with
        | BlockExpr (_, es) -> 
            if block then error (sprintf "\nNested block for expression %s" (format e))
            for (e1, e2) in es do 
                match aux true e1, aux true e2 with
                | PredicateType, RegexType -> ()
                | _, _ -> error (sprintf "\nInvalid block expression %s" (format e))
            BlockType
        | LinkExpr (_, e1, e2) -> 
            match aux block e1, aux block e2 with 
            | RegexType, RegexType -> LinkType
                //TODO: check single locations
            | _, _ -> error (sprintf "Invalid expression %s, specify links with regular expressions" (format e))
        | DiffExpr (_, e1, e2) -> 
            match aux block e1, aux block e2 with 
            | RegexType, RegexType -> RegexType
            | BlockType, BlockType -> BlockType
            | _, _ -> error (sprintf "Invalid expression %s, expected regular expression or block" (format e))
        | ShrExpr (_, e1, e2) ->
            match aux block e1, aux block e2 with 
            | RegexType, RegexType -> RegexType
            | _, _ -> error (sprintf "Invalid expression %s, expected regular expression" (format e))
        | OrExpr (_, e1, e2)
        | AndExpr (_, e1, e2) ->
            match aux block e1, aux block e2 with 
            | RegexType, RegexType -> RegexType
            | PredicateType, PredicateType -> PredicateType
            | BlockType, BlockType -> BlockType
            | _, _ -> error (sprintf "Invalid expression %s, expected regular expressions or predicates" (format e))
        | StarExpr (_, e1) ->
            if aux block e1 = RegexType then RegexType
            else error (sprintf "Invalid expression %s, expected regular expression" (format e))
        | NotExpr (_, e1) -> 
            match aux block e1 with
            | RegexType -> RegexType
            | PredicateType -> PredicateType
            | _ -> error (sprintf "Invalid expression %s, expected regular expression or predicate" (format e))
        | PrefixLiteral (_,(a,b,c,d,bits)) -> wellFormedPrefix (a,b,c,d,bits); PredicateType
        | True _ | False _ | CommunityLiteral _ -> PredicateType
        | IntLiteral _ -> IntType
        | Ident (_, id, _) -> 
            if builtInConstraints.Contains id then ControlType 
            else RegexType
    aux false e
   
(* Given an expression, which is known to be a regex, 
   recursively push the preferences to the top level.
   Assumes blocks have already been expanded. *)

let rec pushPrefsToTop (e: Expr) : Expr list = 
    match e with 
    | AndExpr (p,x,y) -> merge e (x,y) (fun a b -> AndExpr(p,a,b))
    | OrExpr (p,x,y) -> merge e (x,y) (fun a b -> OrExpr(p,a,b))
    | DiffExpr (p,x,y) -> merge e (x,y) (fun a b -> DiffExpr(p,a,b))
    | StarExpr (_,x) -> mergeSingle e x "star"
    | NotExpr (_,x) -> mergeSingle e x "negation"
    | ShrExpr (_, x,y) -> (pushPrefsToTop x) @ (pushPrefsToTop y)
    | Ident _ -> [e]
    | _ -> failwith "unreachable"
and merge e (x,y) f =
    let xs = pushPrefsToTop x 
    let ys = pushPrefsToTop y
    match xs, ys with
    | [a], _ -> List.map (fun b -> f a b) ys
    | _, [b] -> List.map (fun a -> f a b) xs
    | a::_, b::_ -> 
        let sx, sy, sr = format x, format y, format e 
        error (sprintf "\nInvalid use of preferences in regex \nCannot merge: %s and %s in %s" sx sy sr)
    | _, _ -> failwith "impossible"
and mergeSingle e x op =
    let xs = pushPrefsToTop x
    if xs.Length > 1 then
        error (sprintf "\nInvalid use of preferences in regex \nCannot nest under %s operator: %s" op (format e))
    else xs 

(* Construct an actual regular expression from an 
   expression with regex type. Expands built-in 
   constraints like any and always(...) *)

let rec buildRegex (ast: T) (reb: Regex.REBuilder) (r: Expr) : Regex.LazyT =
    let checkParams id n args =
        let m = List.length args
        if m <> n then 
            parseError (sprintf "\nExpected %d arguments for %s, but received %d" n id m) 
        let args = List.map (buildRegex ast reb) args
        let wf = List.map (Regex.singleLocations (reb.Topo())) args
        if List.exists Option.isNone wf then 
            error (sprintf "\nParameter for %s must refer to locations only" id)
        else List.map (Option.get >> Set.toList) wf
    match r with
    | AndExpr(_,x,y) -> reb.Inter [(buildRegex ast reb x); (buildRegex ast reb y)]
    | OrExpr(_,x,y) -> reb.Union [(buildRegex ast reb x); (buildRegex ast reb y)]
    | DiffExpr(_,x,y) -> reb.Inter [(buildRegex ast reb x); reb.Negate (buildRegex ast reb y)]
    | NotExpr (_,x) -> reb.Negate (buildRegex ast reb x)
    | StarExpr (_,x) -> reb.Star (buildRegex ast reb x)
    | Ident(_, id, args) ->
        match id with
        | "valleyfree" -> let locs = checkParams id args.Length args in reb.ValleyFree locs
        | "start" -> let locs = checkParams id 1 args in reb.Start locs.Head
        | "end" -> let locs = checkParams id 1 args in reb.End locs.Head
        | "enter" -> let locs = checkParams id 1 args in reb.Enter locs.Head
        | "exit" -> let locs = checkParams id 1 args in reb.Exit locs.Head
        | "always" -> let locs = checkParams id 1 args in reb.Always locs.Head
        | "through" -> let locs = checkParams id 1 args in reb.Through locs.Head
        | "avoid" -> let locs = checkParams id 1 args in reb.Avoid locs.Head
        | "internal" -> ignore (checkParams id 0 args); reb.Internal()
        | "external" -> ignore (checkParams id 0 args); reb.External()
        | "any" -> ignore (checkParams id 0 args); reb.Any()
        | "drop" -> ignore (checkParams id 0 args); reb.Empty
        | "in" -> ignore (checkParams id 0 args); reb.Inside 
        | "out" -> ignore (checkParams id 0 args);  reb.Outside
        | l -> 
            if args.Length > 0 then 
                error (sprintf "Undefined macro %s" l)
            else reb.Loc l
    | _ -> failwith "unreachable"

(* Build a concrete predicate from an expression
   that is known the have predicate type *)

let rec buildPredicate (e: Expr) : Predicate.T = 
    match e with 
    | True _ -> Predicate.top
    | False _ -> Predicate.bot
    | AndExpr(_,a,b) -> Predicate.conj (buildPredicate a) (buildPredicate b)
    | OrExpr(_,a,b) -> Predicate.disj (buildPredicate a) (buildPredicate b)
    | NotExpr (_,a) -> Predicate.negate (buildPredicate a)
    | PrefixLiteral (_,(a,b,c,d,bits)) ->
        let adjBits = adjustBits bits
        Predicate.prefix (a,b,c,d) adjBits
    | CommunityLiteral (x,y) ->  Predicate.community (string x + ":" + string y)
    | _ -> failwith "unreachable"

(* Helper getter functions for expressions
   where the kind of value is known *)

let inline getPrefix x = 
    match x with 
    | PrefixLiteral (_,(a,b,c,d,bits)) -> (a,b,c,d,bits)
    | _ -> failwith "unreachable" 

let inline getLinks x = 
    match x with 
    | LinkExpr(_,x,y) -> (x,y)
    | _ -> failwith "unreachable"

let inline getInt x = 
    match x with 
    | IntLiteral (_,i) -> i 
    | _ -> failwith "unreachable"

let inline getComm x = 
    match x with 
    | CommunityLiteral (_,(a,b)) -> (a,b)
    | _ -> failwith "unreachable"

(* Build the control constraints
   and extract concrete parameter values *)

let rec checkArgs ast name args = 
    match Map.tryFind name paramInfo with
    | None -> error (sprintf "\nUnrecognized constraint: %s" name)
    | Some (n, typs) -> 
        let len = List.length args
        if len <> n then 
            error (sprintf "\nInvalid number of parameters to: %s \nExpected: %d, but received %d" name n len)
        let mutable i = 0
        for (x,y) in List.zip args typs do 
            i <- i + 1
            match wellFormed x, x, y with
            | PredicateType _, _, PrefixValue -> ()
            | PredicateType, CommunityLiteral _, CommunityValue -> ()
            | LinkType, _, LinkValue -> ()
            | IntType, _, IntValue -> ()
            | _, _, _ -> error (sprintf "\nExpected parameter %d of constraint (%s) to be of type: %s" i name (string y))

let buildCConstraint ast (topo: Topology.T) cc =
    let reb = Regex.REBuilder(topo) 
    let (name, args) = cc
    let args = List.map (fun e -> substitute ast.Defs e) args
    checkArgs ast name args
    let inline prefix x = 
        let (a,b,c,d,bits) as p = getPrefix x
        wellFormedPrefix p
        Prefix.prefix (a,b,c,d) (adjustBits bits)
    let inline getLinkLocations (x,y) =
        let locsX = Regex.singleLocations (reb.Topo()) (buildRegex ast reb x)
        let locsY = Regex.singleLocations (reb.Topo()) (buildRegex ast reb y)
        match locsX, locsY with 
        | Some xs, Some ys -> (xs,ys)
        | _, _ -> error (sprintf "\nLink expression must denote single locations in parameter to: %s" name)
    match name with
    | "aggregate" ->
        let p = prefix (List.head args)
        let (x,y) = getLinks (List.item 1 args)
        let (xs,ys) = getLinkLocations (x,y)
        CAggregate ([p], xs, ys)
    | "tag" -> 
        let (a,b) = getComm (List.head args)
        let p = prefix (List.item 1 args)
        let (x,y) = getLinks (List.item 2 args)
        let (xs,ys) = getLinkLocations (x,y)
        let str = (string a) + ":" + (string b)
        CCommunity (str, [p], xs, ys)
    | "maxroutes" ->
        let i = getInt (List.head args)
        let (x,y) = getLinks (List.item 1 args)
        let (xs,ys) = getLinkLocations (x,y)
        CMaxRoutes (i,xs,ys)
    | "longest_path" -> 
        let i = getInt (List.head args)
        CLongestPath i
    | _ -> failwith "unreachable"

let getControlConstraints (ast: T) (topo: Topology.T) : CConstraint list = 
    List.map (buildCConstraint ast topo) ast.CConstraints

(* Expand blocks in a regular expression to a
   single top-level block, and check if the 
   prefixes cover all cases. *)

type BinOp = 
    | OInter 
    | OUnion 
    | ODifference

let applyOp r1 r2 op = 
    match op with
    | OInter -> AndExpr(dummyPos,r1,r2)
    | OUnion -> OrExpr(dummyPos,r1,r2)
    | ODifference -> DiffExpr(dummyPos,r1,r2)

let checkBlock pcs =
    let mutable remaining = Predicate.top
    let mutable matched = Predicate.bot
    for (pred, es) in pcs do
        let p = Predicate.conj pred remaining
        remaining <- Predicate.conj remaining (Predicate.negate p)
        let p' = Predicate.disj pred matched
        if p' = matched then
            warning (sprintf "Dead prefix %s will never apply" (string p'))
        matched <- p'

    if remaining <> Predicate.bot then
        // let s = Predicate.example rollingPred
        // warning (sprintf "\nIncomplete prefixes in block \nAn example of a prefix that is not matched: %s" s)
        pcs @ [(Predicate.top, [Ident (dummyPos,"any", [])])]
    else pcs

let combineBlocks pcs1 pcs2 (op: BinOp) =
    let mutable combined = []
    let mutable rollingPred = Predicate.bot
    for (ps, res) in pcs1 do 
        for (ps', res') in pcs2 do
            let comb = Predicate.conj ps ps'
            let notAbove = Predicate.conj comb (Predicate.negate rollingPred)
            if notAbove <> Predicate.bot then
                rollingPred <- Predicate.disj rollingPred comb
                let both = (comb, applyOp res res' op)
                combined <- both :: combined
    List.rev combined

let inline collapsePrefs (es: Expr list) : Expr = 
    Common.List.fold1 (fun e1 e2 -> ShrExpr (dummyPos,e1,e2)) es

let rec expandBlocks (e: Expr)  = 
    match e with
    | OrExpr (_,e1,e2) -> combineBlocks (expandBlocks e1) (expandBlocks e2) OUnion
    | AndExpr (_,e1,e2) -> combineBlocks (expandBlocks e1) (expandBlocks e2) OInter
    | DiffExpr (_,e1,e2) -> combineBlocks (expandBlocks e1) (expandBlocks e2) ODifference
    | BlockExpr (_,es) ->  
        List.map (fun (x,y) -> (buildPredicate x, pushPrefsToTop y)) es
        |> checkBlock
        |> List.map (fun (p,es) -> (p, collapsePrefs es))
    | _ -> failwith "unreachable"

(* Given the AST and the topology, check well-formedness
   and produce the top-level, merged path constraints. *)

let makePolicyPairs (ast: T) (topo: Topology.T) : PolicyPair list =
    match Map.tryFind "main" ast.Defs with 
    | Some ([],e) ->
        warnUnused ast e
        let e = substitute ast.Defs e
        match wellFormed e with
        | BlockType -> ()
        | typ ->
            error (sprintf "Expected block in main, but got an expression with type %s" (string typ))
        let topLevel = 
            expandBlocks e
            |> List.map (fun (p,e) -> (p, pushPrefsToTop e))
        List.map (fun (p, res) -> 
            let reb = Regex.REBuilder(topo)
            let res = List.map (buildRegex ast reb) res
            let res = List.map reb.Build res
            (p, reb, res) ) topLevel
    | _ -> error (sprintf "Main policy not defined, use define main = ...")
