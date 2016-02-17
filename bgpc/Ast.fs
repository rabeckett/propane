module Ast

open Topology
open Common.Color

type Position = 
    {SLine: int;
     SCol: int;
     ELine: int;
     ECol: int;}

type Ident = 
    {Pos: Position;
     Name: string}

type Expr =
    {Pos: Position;
     Node: Node}
and Node =
    | Ident of Ident * Expr list
    | BlockExpr of (Expr * Expr) list
    | LinkExpr of Expr * Expr
    | DiffExpr of Expr * Expr
    | ShrExpr of Expr * Expr
    | OrExpr of Expr * Expr
    | AndExpr of Expr * Expr
    | NotExpr of Expr
    | PrefixLiteral of uint32 * uint32 * uint32 * uint32 * uint32 option
    | CommunityLiteral of uint32 * uint32
    | IntLiteral of uint32
    | True
    | False

type Type = 
    | LinkType
    | LocType
    | RegexType
    | PredicateType
    | IntType
    | BlockType

    override this.ToString() = 
        match this with
        | LinkType -> "Links"
        | LocType -> "Location"
        | RegexType -> "Constraint"
        | PredicateType -> "Predicate"
        | IntType -> "Int"
        | BlockType -> "Block"

type Value = 
    | PrefixValue
    | CommunityValue
    | LinkValue
    | IntValue
    | LocValue

    override this.ToString() = 
        match this with
        | PrefixValue -> "Predicate"
        | CommunityValue -> "Community"
        | LinkValue -> "Links"
        | IntValue -> "Int"
        | LocValue -> "Location"

type ControlConstraints = (Ident * Expr list) list
type Definitions = Map<string, Position * Ident list * Expr>

type CConstraint = 
    | CAggregate of Prefix.T list * Set<string> * Set<string>
    | CCommunity of string * Prefix.T list * Set<string> * Set<string>
    | CMaxRoutes of uint32 * Set<string> * Set<string>
    | CLongestPath of uint32 * Set<string>

type PolicyPair = (Predicate.T * Regex.REBuilder * Regex.T list)

type T = 
    {Input: string [];
     Defs: Definitions;
     CConstraints: ControlConstraints}


(* Built-in control constraints and path constraints.
   May add a way for the user to define new constraints. *)

let paramInfo = 
    Map.ofList 
        [("aggregate", [PrefixValue; LinkValue]);
         ("tag", [CommunityValue; PrefixValue; LinkValue]);
         ("maxroutes", [IntValue; LinkValue]);
         ("longest_path", [IntValue; LocValue]) ]

let builtInLocs = 
    Set.ofList ["in"; "out"]

let builtInSingle = 
    Set.add "drop" builtInLocs

let builtInRes = 
    Set.ofList 
        ["start"; "end"; "enter"; 
         "exit"; "valleyfree"; "always"; 
         "through"; "avoid"; "internal"; 
         "any"; "drop"; "in"; "out"]

let builtInConstraints = 
    Set.ofList 
        ["aggregate"; "tag"; 
         "maxroutes"; "longest_path"]

let builtIns = Set.union builtInRes builtInConstraints

(* Helper functions for traversing an AST expression 
   and applying a user-defined function f at each node *)

let rec iter f (e: Expr) =
    f e
    match e.Node with
    | BlockExpr es -> List.iter (fun (e1,e2) -> iter f e1; iter f e2) es
    | LinkExpr (e1, e2)  
    | DiffExpr (e1, e2)  
    | ShrExpr (e1, e2) 
    | OrExpr (e1, e2) 
    | AndExpr (e1, e2) -> iter f e1; iter f e2
    | NotExpr e1 -> iter f e1
    | True | False | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ -> ()
    | Ident (id, args) -> List.iter f args

let getAS (s: string) : uint32 option =
    if s.Length > 2 then
        let n = s.[2..]
        let mutable i = uint32 0
        if System.UInt32.TryParse(n, &i) then
            Some i
        else None
    else None

(* Pretty printing of error and warning messages. 
   When errors occur on a single line, display the line 
   and underline the problem. When errors are multi-line,
   then display the source lines but no underlining. 
   In either case, source line numbers are displayed
   next to the error/warning messages. *)

module Message = 

    open System
    open Common.Color

    type Kind = 
        | Error 
        | Warning

    let msgOffset = 9

    let obj = new Object()

    let inline count (c: char) (s: string) = 
        s.Split(c).Length - 1

    let inline firstNonSpace (s: string) : int = 
        Array.findIndex 
            (fun c -> c <> ' ' && c <> '\t') 
            (s.ToCharArray())

    let displayLine (s:string) (maxLineNo: int) (line: int) =
        let sl = string line 
        let spaces = String.replicate (maxLineNo - sl.Length + 1) " "
        let sl = sl + spaces + "|"
        let len = sl.Length 
        writeColor sl ConsoleColor.DarkGray
        printf "%s%s" (String.replicate (msgOffset-len) " ") s

    let displayMultiLine ast p ccolor =
        let mutable longestLineNo = 0
        let mutable minLineStart = Int32.MaxValue
        for i = p.SLine to p.ELine do 
            let str = ast.Input.[i-1]
            longestLineNo <- max (string i).Length longestLineNo
            minLineStart <- min minLineStart (firstNonSpace str)
        for i = p.SLine to p.ELine do 
            let str = ast.Input.[i-1]
            let str = str.[minLineStart..]
            displayLine str longestLineNo i
            printfn ""
        printfn ""

    let displaySingleLine ast p ccolor =
        let str = ast.Input.[p.SLine-1]
        let lineStart = firstNonSpace str
        let str = str.Trim()
        let spaces = String.replicate (p.SCol - lineStart + msgOffset) " "
        let s = String.replicate (p.ECol - p.SCol) "~"
        let len = (string p.SLine).Length
        displayLine str len p.SLine
        printf "\n%s" spaces
        writeColor s ccolor
        printfn ""

    let displayFooter msg (color, errorTyp) = 
        Common.Color.writeColor errorTyp color
        printfn "%s" (wrapText errorTyp.Length msg)
        writeFooter ()

    let colorInfo (kind: Kind) =
        match kind with
        | Warning -> ConsoleColor.DarkYellow, "Warning: "
        | Error -> ConsoleColor.DarkRed, "Error:   "

    let issueAst (ast: T) (msg: string) (p: Position) (kind: Kind) =
        let ccolor, errorTyp = colorInfo kind
        writeHeader ()
        if p.SLine = p.ELine 
        then displaySingleLine ast p ccolor
        else displayMultiLine ast p ccolor
        displayFooter msg (ccolor, errorTyp)

    let errorAst ast msg p = 
        lock obj (fun () -> 
            issueAst ast msg p Error
            exit 0)

    let warningAst ast msg p = 
        lock obj (fun () -> issueAst ast msg p Warning)


(* Compiler warnings for a variety of common mistakes. 
   Unused definitions or parameters not starting with '_',
   TODO: unused aggregates, etc. *)

let getUsed seen e =
    match e.Node with
    | Ident(id, _) -> 
        seen := Set.add id.Name !seen
    | _ -> ()

let warnUnusedDefs ast e =
    let used = ref Set.empty
    iter (getUsed used) e
    Map.iter (fun _ (_,_,e) -> iter (getUsed used) e) ast.Defs
    let defs = Map.fold (fun acc k (p,_,_) -> Map.add k p acc) Map.empty ast.Defs
    Map.iter (fun id p -> 
        if id <> "main" && id.[0] <> '_' && not (Set.contains id !used) then
            let msg = sprintf "Unused definition of '%s'" id
            Message.warningAst ast msg p) defs


let warnUnusedParams (ast: T) = 
    Map.iter (fun id (_,ps,e) ->
        let seen = ref (Set.empty)
        iter (getUsed seen) e
        for p in ps do
            if p.Name.[0] <> '_' && not (Set.contains p.Name !seen) then
                let msg = sprintf "Unused parameter '%s' in definition of '%s'" p.Name id
                Message.warningAst ast msg p.Pos 
        ) ast.Defs

let warnUnused (ast: T) (e: Expr) : unit = 
    warnUnusedDefs ast e
    warnUnusedParams ast

(* Fully expand all definitions in an expression. 
   Keep track of seen expansions to prevent recursive definitions.
   The position of the new, expanded block is set to the old expression. *)

let substitute (ast: T) (e: Expr) : Expr = 
    let rec aux defs seen e : Expr =
        match e.Node with
        | BlockExpr es -> {Pos=e.Pos;Node=BlockExpr (List.map (fun (e1,e2) -> (aux defs seen e1, aux defs seen e2)) es)}
        | LinkExpr (e1, e2) -> {Pos=e.Pos;Node=LinkExpr (aux defs seen e1, aux defs seen e2)}
        | DiffExpr (e1, e2) -> {Pos=e.Pos;Node=DiffExpr (aux defs seen e1, aux defs seen e2)}
        | ShrExpr (e1, e2) -> {Pos=e.Pos;Node=ShrExpr (aux defs seen e1, aux defs seen e2)}
        | OrExpr (e1, e2) -> {Pos=e.Pos;Node=OrExpr (aux defs seen e1, aux defs seen e2)}
        | AndExpr (e1, e2) -> {Pos=e.Pos;Node=AndExpr (aux defs seen e1, aux defs seen e2)}
        | NotExpr e1 -> {Pos=e.Pos;Node=NotExpr (aux defs seen e1)}
        | True | False | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ -> e
        | Ident (id, []) ->
            if Set.contains id.Name seen then 
                Message.errorAst ast (sprintf "Recursive definition of %s" id.Name) id.Pos
            match Map.tryFind id.Name defs with
            | None -> 
                if builtInSingle.Contains id.Name || Option.isSome (getAS id.Name) then e 
                else Message.errorAst ast (sprintf "Unbound variable '%s'" id.Name) id.Pos
            | Some (_,_,e1) -> aux defs (Set.add id.Name seen) e1
        | Ident (id,es) ->
            if Set.contains id.Name seen then 
                Message.errorAst ast (sprintf "Recursive definition of %s" id.Name) id.Pos
            match Map.tryFind id.Name defs with
            | None ->
                if builtInRes.Contains id.Name then 
                    {Pos=e.Pos;Node=Ident(id, List.map (aux defs seen) es)}
                else Message.errorAst ast (sprintf "Undefined function '%s'" id.Name) id.Pos
            | Some (_,ids,e1) -> 
                let required = List.length ids 
                let provided = List.length es
                if required <> provided then
                    let msg = sprintf "Expected %d parameters to '%s', but only received %d" required id.Name provided
                    Message.errorAst ast msg e.Pos
                let defs' =
                    List.zip ids es
                    |> List.fold (fun acc (id,e) -> Map.add id.Name (e.Pos, [], e) acc) defs
                aux defs' (Set.add id.Name seen) e1
    aux ast.Defs Set.empty e

(* Test well-formedness of an expression. 
   Ensures expressions are of the proper type, 
   that prefixes and links are valid, and 
   that blocks are never nested inside each other. *)

let inline adjustBits bits = 
    match bits with 
    | None -> 32u
    | Some b -> b

let wellFormedPrefix ast pos (a,b,c,d,bits) =
    let bits = adjustBits bits
    let p = Prefix.prefix (a,b,c,d) bits
    if (a > 255u || b > 255u || c > 255u || d > 255u || (bits > 32u)) then
        let msg = sprintf "Found an invalid prefix %s, must be [0-255].[0-255].[0-255].[0-255]/[0-32] " (string p)
        Message.errorAst ast msg pos

let typeMsg (expected: Type list) (t1: Type) = 
    let t = Common.List.joinBy " or " (List.map string expected)
    sprintf "Invalid type, expected %s, but got %s" 
        (string t) 
        (string t1)

let typeMsg2 (expected: Type list) (t1: Type) (t2: Type) = 
    let t = Common.List.joinBy " or " (List.map string expected)
    sprintf "Invalid type, expected %s, but got %s and %s" 
        (string t) 
        (string t1)
        (string t2)

let wellFormed ast (e: Expr) : Type =
    let rec aux block e =
        match e.Node with
        | BlockExpr es -> 
            if block then 
                let msg = "Invalid syntax, nested block expression found"
                Message.errorAst ast msg e.Pos
            for (e1, e2) in es do 
                let t1, t2 = aux true e1, aux true e2
                match t1, t2 with
                | PredicateType, RegexType
                | PredicateType, LocType -> ()
                | PredicateType, _ -> Message.errorAst ast (typeMsg [RegexType] t2) e2.Pos
                | _, _ ->  Message.errorAst ast (typeMsg [PredicateType] t1) e1.Pos
            BlockType
        | LinkExpr (e1, e2) -> 
            let t1 = aux block e1
            let t2 = aux block e2
            match t1, t2 with
            | LocType, LocType -> LinkType
            | _, LocType -> Message.errorAst ast (typeMsg [LocType] t1) e1.Pos
            | LocType, _ -> Message.errorAst ast (typeMsg [LocType] t2) e2.Pos
            | _, _ -> Message.errorAst ast (typeMsg2 [LocType] t1 t2) e.Pos
        | DiffExpr (e1, e2)
        | ShrExpr (e1, e2) -> 
            let t1 = aux block e1
            let t2 = aux block e2
            match t1, t2 with 
            | LocType, LocType -> LocType
            | LocType, RegexType
            | RegexType, LocType
            | RegexType, RegexType -> RegexType
            | BlockType, BlockType -> BlockType
            | _, BlockType -> Message.errorAst ast (typeMsg [BlockType] t1) e1.Pos
            | BlockType, _ -> Message.errorAst ast (typeMsg [BlockType] t2) e2.Pos
            | _, RegexType 
            | _, LocType -> Message.errorAst ast (typeMsg [RegexType] t1) e1.Pos
            | RegexType, _
            | LocType, _ -> Message.errorAst ast (typeMsg [RegexType] t2) e2.Pos
            | _, _ -> Message.errorAst ast (typeMsg2 [BlockType; RegexType] t1 t2) e.Pos
        | OrExpr (e1, e2)
        | AndExpr (e1, e2) ->
            let t1 = aux block e1
            let t2 = aux block e2
            match t1, t2 with 
            | LocType, LocType -> LocType
            | LocType, RegexType
            | RegexType, LocType
            | RegexType, RegexType -> RegexType
            | BlockType, BlockType -> BlockType
            | PredicateType, PredicateType -> PredicateType
            | _, PredicateType -> Message.errorAst ast (typeMsg [PredicateType] t1) e1.Pos
            | PredicateType, _ -> Message.errorAst ast (typeMsg [PredicateType] t2) e2.Pos
            | _, BlockType -> Message.errorAst ast (typeMsg [BlockType] t1) e1.Pos
            | BlockType, _ -> Message.errorAst ast (typeMsg [BlockType] t2) e2.Pos
            | _, RegexType 
            | _, LocType -> Message.errorAst ast (typeMsg [RegexType] t1) e1.Pos
            | RegexType, _
            | LocType, _ -> Message.errorAst ast (typeMsg [RegexType] t2) e2.Pos
            | _, _ -> Message.errorAst ast (typeMsg2 [BlockType; RegexType] t1 t2) e.Pos
        | NotExpr e1 -> 
            let t = aux block e1
            match t with
            | LocType -> LocType
            | RegexType -> RegexType
            | PredicateType -> PredicateType
            | _ -> Message.errorAst ast (typeMsg [RegexType; PredicateType] t) e.Pos
        | PrefixLiteral (a,b,c,d,bits) -> wellFormedPrefix ast e.Pos (a,b,c,d,bits); PredicateType
        | True _ | False _ | CommunityLiteral _ -> PredicateType
        | IntLiteral _ -> IntType
        | Ident (id, args) ->
            match getAS id.Name with 
            | Some i -> LocType 
            | None ->
                if builtInConstraints.Contains id.Name then
                    let msg = sprintf "Invalid control constraint '%s' found in expression" id.Name
                    Message.errorAst ast msg id.Pos
                elif builtInLocs.Contains id.Name then LocType
                elif builtInRes.Contains id.Name then
                    for e in args do 
                        let t = aux block e
                        match t with 
                        | LocType -> ()
                        | _ ->             
                            let msg = (typeMsg [LocType] t)
                            Message.errorAst ast msg e.Pos
                    RegexType 
                else
                    printfn "MADE IT WITH: %s" id.Name 
                    aux block (substitute ast e)
    aux false e
   
(* Given an expression, which is known to be a regex, 
   recursively push the preferences to the top level.
   Assumes blocks have already been expanded. *)

let rec pushPrefsToTop ast (e: Expr) : Expr list = 
    match e.Node with 
    | AndExpr (x,y) -> merge ast e (x,y) (fun a b -> {Pos=e.Pos;Node=AndExpr(a,b)})
    | OrExpr (x,y) -> merge ast e (x,y) (fun a b -> {Pos=e.Pos;Node=OrExpr(a,b)})
    | DiffExpr (x,y) -> merge ast e (x,y) (fun a b -> {Pos=e.Pos;Node=DiffExpr(a,b)})
    | NotExpr x -> mergeSingle ast e x "negation"
    | ShrExpr (x,y) -> (pushPrefsToTop ast x) @ (pushPrefsToTop ast y)
    | Ident _ -> [e]
    | _ -> Common.unreachable ()
and merge ast e (x,y) f =
    let xs = pushPrefsToTop ast x 
    let ys = pushPrefsToTop ast y
    match xs, ys with
    | [a], _ -> List.map (fun b -> f a b) ys
    | _, [b] -> List.map (fun a -> f a b) xs
    | a::_, b::_ ->
        let msg = 
            sprintf "Invalid use of preferences when merging main policy." + 
            sprintf "This can lead to ambiguous preferences unless clarified."
        Message.errorAst ast msg e.Pos
        error msg
    | _, _ -> Common.unreachable ()
and mergeSingle ast e x op =
    let xs = pushPrefsToTop ast x
    if xs.Length > 1 then
        let msg = sprintf "Invalid use of preferences with %s" op
        Message.errorAst ast msg e.Pos
    else xs 

(* Construct an actual regular expression from an 
   expression with regex type. Expands built-in 
   constraints like any and always(...) *)

let rec buildRegex (ast: T) (reb: Regex.REBuilder) (r: Expr) : Regex.LazyT =
    let checkParams id n args =
        let m = List.length args
        if m <> n then
            let msg = sprintf "Expected %d arguments for %s, but received %d" n id.Name m 
            Message.errorAst ast msg id.Pos
        let args = List.map (buildRegex ast reb) args
        let wf = List.map (Regex.singleLocations (reb.Topo())) args
        List.map (Option.get >> Set.toList) wf
    match r.Node with
    | AndExpr(x,y) -> reb.Inter [(buildRegex ast reb x); (buildRegex ast reb y)]
    | OrExpr(x,y) -> reb.Union [(buildRegex ast reb x); (buildRegex ast reb y)]
    | DiffExpr(x,y) -> reb.Inter [(buildRegex ast reb x); reb.Negate (buildRegex ast reb y)]
    | NotExpr x -> reb.Negate (buildRegex ast reb x)
    | Ident(id, args) ->
        match id.Name with
        | "valleyfree" -> let locs = checkParams id args.Length args in reb.ValleyFree locs
        | "start" -> let locs = checkParams id 1 args in reb.Start locs.Head
        | "end" -> let locs = checkParams id 1 args in reb.End locs.Head
        | "enter" -> let locs = checkParams id 1 args in reb.Enter locs.Head
        | "exit" -> let locs = checkParams id 1 args in reb.Exit locs.Head
        | "always" -> let locs = checkParams id 1 args in reb.Always locs.Head
        | "through" -> let locs = checkParams id 1 args in reb.Through locs.Head
        | "avoid" -> let locs = checkParams id 1 args in reb.Avoid locs.Head
        | "internal" -> ignore (checkParams id 0 args); reb.Internal()
        | "any" -> ignore (checkParams id 0 args); reb.Any()
        | "drop" -> ignore (checkParams id 0 args); reb.Empty
        | "in" -> ignore (checkParams id 0 args); reb.Inside 
        | "out" -> ignore (checkParams id 0 args);  reb.Outside
        | l -> 
            if args.Length > 0 then 
                error (sprintf "Undefined macro %s" l)
            else reb.Loc l
    | _ -> Common.unreachable ()

(* Build a concrete predicate from an expression
   that is known the have predicate type *)

let rec buildPredicate (e: Expr) : Predicate.T = 
    match e.Node with 
    | True -> Predicate.top
    | False -> Predicate.bot
    | AndExpr(a,b) -> Predicate.conj (buildPredicate a) (buildPredicate b)
    | OrExpr(a,b) -> Predicate.disj (buildPredicate a) (buildPredicate b)
    | NotExpr a -> Predicate.negate (buildPredicate a)
    | PrefixLiteral (a,b,c,d,bits) ->
        let adjBits = adjustBits bits
        Predicate.prefix (a,b,c,d) adjBits
    | CommunityLiteral (x,y) ->  Predicate.community (string x + ":" + string y)
    | _ -> Common.unreachable ()

(* Helper getter functions for expressions
   where the kind of value is known *)

let inline getPrefix x = 
    match x with 
    | PrefixLiteral (a,b,c,d,bits) -> (a,b,c,d,bits)
    | _ -> Common.unreachable ()

let inline getLinks x = 
    match x with 
    | LinkExpr(x,y) -> (x,y)
    | _ -> Common.unreachable ()

let inline getInt x = 
    match x with 
    | IntLiteral i -> i 
    | _ -> Common.unreachable ()

let inline getComm x = 
    match x with 
    | CommunityLiteral (a,b) -> (a,b)
    | _ -> Common.unreachable ()

(* Build the control constraints
   and extract concrete parameter values *)

let rec wellFormedCCs ast id args argsOrig = 
    match Map.tryFind id.Name paramInfo with
    | None -> 
        let msg = sprintf "Unrecognized constraint '%s'" id.Name
        Message.errorAst ast msg id.Pos
    | Some typs -> 
        let n = typs.Length
        let len = List.length args
        if len <> n then 
            let msg = sprintf "Expected %d parameters to %s, but got %d" n id.Name len
            Message.errorAst ast msg id.Pos
        let mutable i = 0
        for (x,origE,y) in List.zip3 args argsOrig typs do 
            i <- i + 1
            match wellFormed ast x, x.Node, y with
            | PredicateType _, PrefixLiteral _, PrefixValue -> ()
            | PredicateType, CommunityLiteral _, CommunityValue -> ()
            | LinkType, LinkExpr _, LinkValue -> ()
            | IntType, IntLiteral _, IntValue -> ()
            | LocType, _, LocValue -> ()
            | _, _, _ -> 
                let msg = sprintf "Expected parameter %d of %s to be a %s value" i id.Name (string y)
                Message.errorAst ast msg origE.Pos

let buildCConstraint ast (topo: Topology.T) (cc: Ident * Expr list) =
    let reb = Regex.REBuilder(topo) 
    let (id, argsOrig) = cc
    let args = List.map (fun e -> substitute ast e) argsOrig
    wellFormedCCs ast id args argsOrig
    let inline prefix x = 
        let (a,b,c,d,bits) as p = getPrefix x
        Prefix.prefix (a,b,c,d) (adjustBits bits)
    let inline getLinkLocations (x,y) =
        let xs = Regex.singleLocations (reb.Topo()) (buildRegex ast reb x) |> Option.get
        let ys = Regex.singleLocations (reb.Topo()) (buildRegex ast reb y) |> Option.get
        (xs,ys)
    match id.Name with
    | "aggregate" ->
        let p = prefix (List.head args).Node
        let (x,y) = getLinks (List.item 1 args).Node
        let (xs,ys) = getLinkLocations (x,y)
        CAggregate ([p], xs, ys)
    | "tag" -> 
        let (a,b) = getComm (List.head args).Node
        let p = prefix (List.item 1 args).Node
        let (x,y) = getLinks (List.item 2 args).Node
        let (xs,ys) = getLinkLocations (x,y)
        let str = (string a) + ":" + (string b)
        CCommunity (str, [p], xs, ys)
    | "maxroutes" ->
        let i = getInt (List.head args).Node
        let (x,y) = getLinks (List.item 1 args).Node
        let (xs,ys) = getLinkLocations (x,y)
        CMaxRoutes (i,xs,ys)
    | "longest_path" -> 
        let i = getInt (List.head args).Node
        let r = buildRegex ast reb (List.item 1 args)
        let locs = Regex.singleLocations (reb.Topo()) r 
        CLongestPath (i, Option.get locs)
    | _ -> Common.unreachable ()

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
    | OInter -> AndExpr(r1,r2)
    | OUnion -> OrExpr(r1,r2)
    | ODifference -> DiffExpr(r1,r2)

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
        // warning (sprintf "Incomplete prefixes in block. An example of a prefix that is not matched: %s" s)
        let e = pcs |> List.head |> snd |> List.head
        pcs @ [(Predicate.top, [ {Pos=e.Pos; Node=Ident ({Pos=e.Pos;Name="any"}, [])} ])]
    else pcs

let combineBlocks pos pcs1 pcs2 (op: BinOp) =
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
    |> List.map (fun (pred,n) -> pred, {Pos=pos; Node=n})

let inline collapsePrefs pos (es: Expr list) : Expr =
    Common.List.fold1 (fun e1 e2 -> {Pos=pos; Node=ShrExpr (e1,e2)}) es

let rec expandBlocks ast (e: Expr) = 
    match e.Node with
    | OrExpr (e1,e2) -> combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) OUnion
    | AndExpr (e1,e2) -> combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) OInter
    | DiffExpr (e1,e2) -> combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) ODifference
    | BlockExpr es ->  
        List.map (fun (x,y) -> (buildPredicate x, pushPrefsToTop ast y)) es
        |> checkBlock
        |> List.map (fun (p,es) -> (p, collapsePrefs e.Pos es))
    | _ -> Common.unreachable ()

(* Given the AST and the topology, check well-formedness
   and produce the top-level, merged path constraints. *)

let makePolicyPairs (ast: T) (topo: Topology.T) : PolicyPair list =
    match Map.tryFind "main" ast.Defs with 
    | Some (_,[],e) ->
        let e = substitute ast e
        warnUnused ast e
        let t = wellFormed ast e
        match t with
        | BlockType -> ()
        | _ -> Message.errorAst ast (typeMsg [BlockType] t) e.Pos
        Map.iter (fun id (p,args,e) ->
            let e = substitute ast e
            ignore (wellFormed ast e)) ast.Defs
        let topLevel = 
            expandBlocks ast e
            |> List.map (fun (p,e) -> (p, pushPrefsToTop ast e))
        List.map (fun (p, res) -> 
            let reb = Regex.REBuilder(topo)
            let res = List.map (buildRegex ast reb) res
            let res = List.map reb.Build res
            (p, reb, res) ) topLevel
    | _ -> error (sprintf "Main policy not defined, use define main = ...")
