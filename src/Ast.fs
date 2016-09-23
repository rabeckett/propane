module Ast

open Route
open Topology
open Util.Format

type Position = 
   { SLine : int
     SCol : int
     ELine : int
     ECol : int }

type Ident = 
   { Pos : Position
     Name : string }

type Expr = 
   { Pos : Position
     Node : Node }

and Node = 
   | Ident of Ident * Expr list
   | BlockExpr of (Expr * Expr) list
   | LinkExpr of Expr * Expr
   | DiffExpr of Expr * Expr
   | ShrExpr of Expr * Expr
   | OrExpr of Expr * Expr
   | AndExpr of Expr * Expr
   | LOrExpr of Expr * Expr
   | LAndExpr of Expr * Expr
   | NotExpr of Expr
   | TemplateVar of Ident option * Ident
   | PrefixLiteral of int * int * int * int * (int * int) option
   | CommunityLiteral of int * int
   | Asn of int
   | IntLiteral of int
   | Wildcard
   | True
   | False

type Type = 
   | LinkType
   | LocType
   | WildcardType
   | RegexType
   | PredicateType
   | IntType
   | BlockType
   override this.ToString() = 
      match this with
      | LinkType -> "Links"
      | LocType -> "Location"
      | WildcardType -> "Wildcard"
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
      | PrefixValue -> "Prefix"
      | CommunityValue -> "Community"
      | LinkValue -> "Links"
      | IntValue -> "Int"
      | LocValue -> "Location"

type ControlConstraints = (Ident * Expr list) list

type Definitions = Map<string, Position * Ident list * Expr>

type CConstraint = 
   | CAggregate of Prefix * Set<string> * Set<string>
   | CCommunity of string * Prefix list * Set<string> * Set<string>
   | CMaxRoutes of int * Set<string> * Set<string>
   | CLongestPath of int * Set<string>

type PolicyPair = Predicate * Regex.REBuilder * Regex.T list

type T = 
   { Input : string []
     TopoInfo : Topology.TopoInfo
     Defs : Definitions
     CConstraints : ControlConstraints }

(* Built-in control constraints and path constraints.
   May add a way for the user to define new constraints. *)

let paramInfo = 
   Map.ofList [ ("aggregate", [ PrefixValue; LinkValue ])
                ("tag", [ CommunityValue; PrefixValue; LinkValue ])
                ("maxroutes", [ IntValue; LinkValue ])
                ("longest_path", [ IntValue; LocValue ]) ]

let builtInLocs = Set.ofList [ "in"; "out" ]
let builtInSingle = Set.add "drop" builtInLocs
let builtInRes = 
   Set.ofList 
      [ "start"; "end"; "enter"; "exit"; "valleyfree"; "path"; "reach"; "always"; "through"; "avoid"; 
        "internal"; "any"; "drop"; "in"; "out" ]
let builtInExpandable = Set.ofList [ "start"; "end"; "enter"; "exit"; "through"; "avoid" ]
let builtInConstraints = Set.ofList [ "aggregate"; "tag"; "maxroutes"; "longest_path" ]
let builtIns = Set.union builtInRes builtInConstraints
let routerTemplate = "router"

(* Helper functions for traversing an AST expression 
   and applying a user-defined function f at each node *)
let rec iter f (e : Expr) = 
   f e
   match e.Node with
   | BlockExpr es -> 
      List.iter (fun (e1, e2) -> 
         iter f e1
         iter f e2) es
   | LinkExpr(e1, e2) | DiffExpr(e1, e2) | ShrExpr(e1, e2) -> 
      iter f e1
      iter f e2
   | OrExpr(e1, e2) | AndExpr(e1, e2) | LOrExpr(e1, e2) | LAndExpr(e1, e2) -> 
      iter f e1
      iter f e2
   | NotExpr e1 -> iter f e1
   | Ident(id, args) -> List.iter (iter f) args
   | True | False | Wildcard | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ | Asn _ -> ()
   | TemplateVar(_, _) -> ()

let iterAllExpr (ast : T) e f = 
   Map.iter (fun name (_, _, e) -> iter f e) ast.Defs
   iter f e
   List.iter (fun (id, es) -> List.iter (iter f) es) ast.CConstraints

(* Pretty printing of error and warning messages. 
   When errors occur on a single line, display the line 
   and underline the problem. When errors are multi-line,
   then display the source lines but no underlining. 
   In either case, source line numbers are displayed
   next to the error/warning messages. *)

module Message = 
   open System
   
   type Kind = 
      | Error
      | Warning
   
   let dummyPos = 
      { SLine = -1
        SCol = -1
        ELine = -1
        ECol = -1 }
   
   let msgOffset = 9
   let maxLineMsg = 3
   let obj = new Object()
   
   let range (x : Position) (y : Position) = 
      { SLine = x.SLine
        SCol = x.SCol
        ELine = y.ELine
        ECol = y.ECol }
   
   let inline count (c : char) (s : string) = s.Split(c).Length - 1
   let inline firstNonSpace (s : string) : int = 
      Array.findIndex (fun c -> c <> ' ' && c <> '\t') (s.ToCharArray())
   
   let displayLine (s : string option) (maxLineNo : int) (line : int) = 
      let s = 
         match s with
         | None -> "..."
         | Some x -> x
      
      let sl = string line
      let spaces = String.replicate (maxLineNo - sl.Length + 1) " "
      let sl = sl + spaces + "|"
      let len = sl.Length
      writeColor sl ConsoleColor.DarkGray
      printf "%s%s" (String.replicate (msgOffset - len) " ") s
   
   let displayMultiLine ast p ccolor = 
      let mutable longestLineNo = 0
      let mutable minLineStart = Int32.MaxValue
      for i = p.SLine to p.ELine do
         let str = ast.Input.[i - 1]
         longestLineNo <- max (string i).Length longestLineNo
         minLineStart <- min minLineStart (firstNonSpace str)
      let lines = min maxLineMsg (p.ELine - p.SLine)
      for i = p.SLine to p.SLine + lines do
         let str = ast.Input.[i - 1]
         let str = str.[minLineStart..]
         
         let msg = 
            if i = p.SLine + maxLineMsg then None
            else Some str
         displayLine msg longestLineNo i
         printfn ""
      printfn ""
   
   let displaySingleLine ast p ccolor = 
      let str = ast.Input.[p.SLine - 1]
      let lineStart = firstNonSpace str
      let str = str.Trim()
      let spaces = String.replicate (p.SCol - lineStart + msgOffset) " "
      let s = String.replicate (p.ECol - p.SCol) "~"
      let len = (string p.SLine).Length
      displayLine (Some str) len p.SLine
      printf "\n%s" spaces
      writeColor s ccolor
      printfn ""
   
   let displayFooter msg (color, errorTyp) = 
      writeColor errorTyp color
      printfn "%s" (wrapText msg)
      writeFooter()
   
   let colorInfo (kind : Kind) = 
      match kind with
      | Warning -> ConsoleColor.DarkYellow, "Warning: "
      | Error -> ConsoleColor.DarkRed, "Error:   "
   
   let issueAst (ast : T) (msg : string) (p : Position) (kind : Kind) = 
      let settings = Args.getSettings()
      let ccolor, errorTyp = colorInfo kind
      writeHeader()
      if p.SLine = p.ELine then displaySingleLine ast p ccolor
      else displayMultiLine ast p ccolor
      displayFooter msg (ccolor, errorTyp)
   
   let validate p = 
      if p = dummyPos then 
         printfn "Internal positioning error"
         exit 0
   
   let errorAst ast msg p = 
      validate p
      lock obj (fun () -> 
         issueAst ast msg p Error
         exit 0)
   
   let warningAst ast msg p = 
      validate p
      lock obj (fun () -> issueAst ast msg p Warning)

(* Fully expand all definitions in an expression. 
   Keep track of seen expansions to prevent recursive definitions.
   The position of the new, expanded block is set to the old expression. *)

let substitute (ast : T) (e : Expr) : Expr = 
   let rec aux defs seen e : Expr = 
      match e.Node with
      | BlockExpr es -> 
         { Pos = e.Pos
           Node = BlockExpr(List.map (fun (e1, e2) -> (aux defs seen e1, aux defs seen e2)) es) }
      | LinkExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = LinkExpr(aux defs seen e1, aux defs seen e2) }
      | DiffExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = DiffExpr(aux defs seen e1, aux defs seen e2) }
      | ShrExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = ShrExpr(aux defs seen e1, aux defs seen e2) }
      | OrExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = OrExpr(aux defs seen e1, aux defs seen e2) }
      | AndExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = AndExpr(aux defs seen e1, aux defs seen e2) }
      | LOrExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = LOrExpr(aux defs seen e1, aux defs seen e2) }
      | LAndExpr(e1, e2) -> 
         { Pos = e.Pos
           Node = LAndExpr(aux defs seen e1, aux defs seen e2) }
      | NotExpr e1 -> 
         { Pos = e.Pos
           Node = NotExpr(aux defs seen e1) }
      | True | False | Wildcard | PrefixLiteral _ | CommunityLiteral _ | IntLiteral _ | Asn _ -> e
      | TemplateVar(ido, name) -> 
         match ido with
         | None -> e
         | Some id -> 
            match ast.TopoInfo.Kind with
            | Abstract | Template -> 
               if ast.TopoInfo.SelectGraphInfo.InternalNames.Contains id.Name then e
               else 
                  Message.errorAst ast (sprintf "Invalid template variable name %s" id.Name) id.Pos
            | Concrete -> 
               Message.errorAst ast ("Invalid use of template variable for a concrete topology") 
                  id.Pos
      | Ident(id, es) -> 
         if Set.contains id.Name seen then 
            Message.errorAst ast (sprintf "Recursive definition of %s" id.Name) id.Pos
         match Map.tryFind id.Name defs with
         | None -> 
            if builtInSingle.Contains id.Name then e
            elif builtInRes.Contains id.Name then 
               { Pos = e.Pos
                 Node = Ident(id, List.map (aux defs seen) es) }
            else Message.errorAst ast (sprintf "Unbound identifier '%s'" id.Name) id.Pos
         | Some(_, ids, e1) -> 
            let req, prov = List.length ids, List.length es
            if req <> prov then 
               let msg = 
                  sprintf "Expected %d parameters to '%s', but only received %d" req id.Name prov
               Message.errorAst ast msg e.Pos
            let defs' = 
               List.zip ids es 
               |> List.fold (fun acc (id, e) -> Map.add id.Name (e.Pos, [], e) acc) defs
            aux defs' (Set.add id.Name seen) e1
   aux ast.Defs Set.empty e

(* Test well-formedness of an expression. 
   Ensures expressions are of the proper type, 
   that prefixes and links are valid, and 
   that blocks are never nested inside each other. *)

let inline adjustBits bits = 
   match bits with
   | None -> (32, 32)
   | Some b -> b

let createPrefix (a, b, c, d, lo, hi) = 
   if hi = -1 then Prefix(a, b, c, d, lo)
   else Prefix(a, b, c, d, lo, Range(lo, hi))

let wellFormedPrefix ast pos (a, b, c, d, bits) = 
   let (lo, hi) = adjustBits bits
   if (a > 255 || b > 255 || c > 255 || d > 255 || (lo > 32) || (hi > 32)) then 
      let p = Prefix(a, b, c, d, lo, Range(lo, hi))
      let msg = 
         sprintf "Found an invalid prefix %s, " (string p) 
         + sprintf "must match (0-255).(0-255).(0-255).(0-255)/[(0-32)..(0-32)]"
      Message.errorAst ast msg pos

let typeMsg (expected : Type list) (t1 : Type) = 
   let t = Util.List.joinBy " or " (List.map string expected)
   sprintf "Invalid type, expected %s, but got %s" (string t) (string t1)

let typeMsg2 (expected : Type list) (t1 : Type) (t2 : Type) = 
   let t = Util.List.joinBy " or " (List.map string expected)
   sprintf "Invalid type, expected 2 of (%s), but got %s and %s" (string t) (string t1) (string t2)

let wellFormed ast (e : Expr) : Type = 
   let settings = Args.getSettings()
   
   let rec aux block e = 
      match e.Node with
      | BlockExpr es -> 
         if block then 
            let msg = "Invalid syntax, nested block expression found"
            Message.errorAst ast msg e.Pos
         for (e1, e2) in es do
            let t1, t2 = aux true e1, aux true e2
            match t1, t2 with
            | PredicateType, RegexType -> ()
            | PredicateType, _ -> Message.errorAst ast (typeMsg [ RegexType ] t2) e2.Pos
            | _, _ -> Message.errorAst ast (typeMsg [ PredicateType ] t1) e1.Pos
         BlockType
      | LinkExpr(e1, e2) -> 
         let t1 = aux block e1
         let t2 = aux block e2
         match t1, t2 with
         | LocType, LocType -> LinkType
         | _, LocType -> Message.errorAst ast (typeMsg [ LocType ] t1) e1.Pos
         | LocType, _ -> Message.errorAst ast (typeMsg [ LocType ] t2) e2.Pos
         | _, _ -> Message.errorAst ast (typeMsg2 [ LocType ] t1 t2) e.Pos
      | DiffExpr(e1, e2) | ShrExpr(e1, e2) -> 
         let t1 = aux block e1
         let t2 = aux block e2
         match t1, t2 with
         | LocType, LocType -> LocType
         | LocType, RegexType | RegexType, LocType | RegexType, RegexType -> RegexType
         | BlockType, BlockType -> BlockType
         | _, _ -> Message.errorAst ast (typeMsg2 [ BlockType; RegexType; LocType ] t1 t2) e.Pos
      | OrExpr(e1, e2) | AndExpr(e1, e2) -> 
         let t1 = aux block e1
         let t2 = aux block e2
         match t1, t2 with
         | PredicateType, PredicateType -> PredicateType
         | _, _ -> Message.errorAst ast (typeMsg2 [ PredicateType ] t1 t2) e.Pos
      | LOrExpr(e1, e2) | LAndExpr(e1, e2) -> 
         let t1 = aux block e1
         let t2 = aux block e2
         match t1, t2 with
         | LocType, LocType -> LocType
         | LocType, RegexType | RegexType, LocType | RegexType, RegexType -> RegexType
         | BlockType, BlockType -> BlockType
         | _, _ -> Message.errorAst ast (typeMsg2 [ BlockType; RegexType; LocType ] t1 t2) e.Pos
      | NotExpr e1 -> 
         let t = aux block e1
         match t with
         | LocType -> LocType
         | RegexType -> RegexType
         | PredicateType -> 
            let msg = 
               sprintf "Using explicit negation with predicates is not allowed. " 
               + sprintf "Implicit negation through ordering is required."
            Message.errorAst ast msg e.Pos
         | _ -> Message.errorAst ast (typeMsg [ RegexType; PredicateType ] t) e.Pos
      | PrefixLiteral(a, b, c, d, bits) -> 
         wellFormedPrefix ast e.Pos (a, b, c, d, bits)
         PredicateType
      | True _ | False _ | CommunityLiteral _ -> PredicateType
      | Wildcard -> WildcardType
      | IntLiteral _ -> IntType
      | Asn _ -> LocType
      | TemplateVar(ido, id) -> 
         if not settings.IsAbstract then 
            let msg = 
               sprintf "Template variable: $%s$ detected, but abstract compilation " id.Name 
               + sprintf "is not enabled. Use the -abstract:on flag to enable abstract compilation"
            Message.errorAst ast msg id.Pos
         if id.Name = routerTemplate then LocType
         else PredicateType
      | Ident(id, args) -> 
         if builtInConstraints.Contains id.Name then 
            let msg = sprintf "Invalid control constraint '%s' found in expression" id.Name
            Message.errorAst ast msg id.Pos
         elif builtInLocs.Contains id.Name then LocType
         elif builtInRes.Contains id.Name then 
            for e in args do
               let t = aux block e
               match t with
               | LocType -> ()
               | WildcardType -> 
                  if id.Name <> "path" then 
                     Message.errorAst ast (typeMsg [ LocType ] WildcardType) e.Pos
               | _ -> Message.errorAst ast (typeMsg [ LocType ] t) e.Pos
            RegexType
         else aux block (substitute ast e)
   aux false e

(* Given an expression, which is known to be a regex, 
   recursively push the preferences to the top level.
   Assumes blocks have already been expanded. *)

let rec pushPrefsToTop ast (e : Expr) : Expr list = 
   match e.Node with
   | LAndExpr(x, y) -> 
      merge ast e (x, y) (fun a b -> 
         { Pos = e.Pos
           Node = LAndExpr(a, b) })
   | LOrExpr(x, y) -> 
      merge ast e (x, y) (fun a b -> 
         { Pos = e.Pos
           Node = LOrExpr(a, b) })
   | DiffExpr(x, y) -> 
      merge ast e (x, y) (fun a b -> 
         { Pos = e.Pos
           Node = DiffExpr(a, b) })
   | NotExpr x -> mergeSingle ast e x "negation"
   | ShrExpr(x, y) -> (pushPrefsToTop ast x) @ (pushPrefsToTop ast y)
   | Ident(id, es) -> 
      if builtInExpandable.Contains id.Name then 
         match es with
         | [ e' ] -> 
            let es = pushPrefsToTop ast e'
            List.map (fun e' -> 
               { Pos = e.Pos
                 Node = Ident(id, [ e' ]) }) es
         | es -> Util.unreachable()
      else [ e ]
   | Asn _ | TemplateVar _ -> [ e ]
   | _ -> Util.unreachable()

and merge ast e (x, y) f = 
   let xs = pushPrefsToTop ast x
   let ys = pushPrefsToTop ast y
   match xs, ys with
   | [ a ], _ -> List.map (fun b -> f a b) ys
   | _, [ b ] -> List.map (fun a -> f a b) xs
   | a :: _, b :: _ -> 
      let msg = 
         sprintf "Invalid use of preferences when merging main policy." 
         + sprintf "This can lead to ambiguous preferences unless clarified."
      Message.errorAst ast msg e.Pos
      error msg
   | _, _ -> Util.unreachable()

and mergeSingle ast e x op = 
   let xs = pushPrefsToTop ast x
   if xs.Length > 1 then 
      let msg = sprintf "Invalid use of preferences with %s" op
      Message.errorAst ast msg e.Pos
   else [ e ]

let getAsnForTemplateVar ast (id) = 
   match Map.tryFind id.Name ast.Defs with
   | None -> failwith "Invariant violation"
   | Some(_, _, e) -> 
      match e.Node with
      | Asn i -> string i
      | _ -> failwith "Invariant violation"

(* Construct an actual regular expression from an 
   expression with regex type. Expands built-in 
   constraints like any and always(...) *)

let rec buildRegex (ast : T) (reb : Regex.REBuilder) (r : Expr) : Regex.LazyT = 
   let checkParams id n args = 
      let m = List.length args
      if m <> n then 
         let msg = sprintf "Expected %d arguments for %s, but received %d" n id.Name m
         Message.errorAst ast msg id.Pos
      let args = List.map (buildRegex ast reb) args
      let wf = List.map reb.SingleLocations args
      List.map (Option.get >> Set.toList) wf
   match r.Node with
   | LAndExpr(x, y) -> 
      reb.Inter [ (buildRegex ast reb x)
                  (buildRegex ast reb y) ]
   | LOrExpr(x, y) -> 
      reb.Union [ (buildRegex ast reb x)
                  (buildRegex ast reb y) ]
   | DiffExpr(x, y) -> 
      reb.Inter [ (buildRegex ast reb x)
                  reb.Negate(buildRegex ast reb y) ]
   | NotExpr x -> reb.Negate(buildRegex ast reb x)
   | Asn i -> reb.Loc(string i)
   | Wildcard -> reb.Wildcard
   | Ident(id, args) -> 
      match id.Name with
      | "reach" -> 
         let locs = checkParams id 2 args
         reb.Reach(locs.[0], locs.[1])
      | "path" -> 
         let locs = checkParams id args.Length args
         // The wildcard "_" only appears by itself because of checking well-formedness
         reb.Path <| locs
      | "valleyfree" -> 
         let locs = checkParams id args.Length args
         reb.ValleyFree locs
      | "start" -> 
         let locs = checkParams id 1 args
         reb.Start locs.Head
      | "end" -> 
         let locs = checkParams id 1 args
         reb.End locs.Head
      | "enter" -> 
         let locs = checkParams id 1 args
         reb.Enter locs.Head
      | "exit" -> 
         let locs = checkParams id 1 args
         reb.Exit locs.Head
      | "always" -> 
         let locs = checkParams id 1 args
         reb.Always locs.Head
      | "through" -> 
         let locs = checkParams id 1 args
         reb.Through locs.Head
      | "avoid" -> 
         let locs = checkParams id 1 args
         reb.Avoid locs.Head
      | "internal" -> 
         ignore (checkParams id 0 args)
         reb.Internal()
      | "any" -> 
         ignore (checkParams id 0 args)
         reb.Any()
      | "drop" -> 
         ignore (checkParams id 0 args)
         reb.Empty
      | "in" -> 
         ignore (checkParams id 0 args)
         reb.Inside
      | "out" -> 
         ignore (checkParams id 0 args)
         reb.Outside
      | _ -> Util.unreachable()
   | ShrExpr(e1, e2) -> 
      Message.errorAst ast (sprintf "Preferences in built-in constraint currently not handled") 
         r.Pos
   | TemplateVar(ido, id) -> 
      match ido with
      | None -> 
         let msg = sprintf "Template router variable %s is not bound to an abstract router" id.Name
         Message.errorAst ast msg r.Pos
      | Some x -> reb.Loc(getAsnForTemplateVar ast x)
   | BlockExpr _ | LinkExpr _ | IntLiteral _ | CommunityLiteral _ | PrefixLiteral _ | OrExpr _ | AndExpr _ | False | True -> 
      Util.unreachable()

(* Build a concrete predicate from an expression
   that is known the have predicate type *)

let rec buildPredicate (e : Expr) : Predicate = 
   match e.Node with
   | True -> Route.top
   | False -> Route.bot
   | AndExpr(a, b) -> Route.conj (buildPredicate a) (buildPredicate b)
   | OrExpr(a, b) -> Route.disj (buildPredicate a) (buildPredicate b)
   | PrefixLiteral(a, b, c, d, bits) -> 
      let (lo, hi) = adjustBits bits
      let p = createPrefix (a, b, c, d, lo, hi)
      Route.prefix p
   | CommunityLiteral(x, y) -> Route.community (string x + ":" + string y)
   | TemplateVar(ido, id) -> 
      match ido with
      | None -> Route.templateVar (sprintf "global.$%s$" id.Name)
      | Some x -> Route.templateVar (sprintf "%s.$%s$" x.Name id.Name)
   | Wildcard | BlockExpr _ | DiffExpr _ | IntLiteral _ | NotExpr _ | LinkExpr _ | ShrExpr _ | LAndExpr _ | LOrExpr _ | Ident _ | Asn _ -> 
      Util.unreachable()

(* Helper getter functions for expressions
   where the kind of value is known *)

let inline getPrefix x = 
   match x with
   | PrefixLiteral(a, b, c, d, bits) -> 
      let (lo, hi) = adjustBits bits
      createPrefix (a, b, c, d, lo, hi)
   | TemplateVar(ido, id) -> 
      match ido with
      | None -> Prefix(sprintf "$%s$" id.Name)
      | Some x -> Prefix(sprintf "%s.$%s$" x.Name id.Name)
   | _ -> Util.unreachable()

let inline getLinks x = 
   match x with
   | LinkExpr(x, y) -> (x, y)
   | _ -> Util.unreachable()

let inline getInt x = 
   match x with
   | IntLiteral i -> i
   | _ -> Util.unreachable()

let inline getComm x = 
   match x with
   | CommunityLiteral(a, b) -> (a, b)
   | _ -> Util.unreachable()

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
      for (x, origE, y) in List.zip3 args argsOrig typs do
         i <- i + 1
         match wellFormed ast x, x.Node, y with
         | LinkType, LinkExpr _, LinkValue -> ()
         | IntType, IntLiteral _, IntValue -> ()
         | LocType, _, LocValue -> ()
         | PredicateType _, PrefixLiteral _, PrefixValue -> ()
         | PredicateType, TemplateVar(_, _), PrefixValue -> ()
         | PredicateType, CommunityLiteral _, CommunityValue -> ()
         | _, _, _ -> 
            let msg = sprintf "Expected parameter %d of %s to be a %s value" i id.Name (string y)
            Message.errorAst ast msg origE.Pos

let buildCConstraint ast (cc : Ident * Expr list) = 
   let reb = Regex.REBuilder(ast.TopoInfo.SelectGraphInfo.Graph)
   let (id, argsOrig) = cc
   let args = List.map (fun e -> substitute ast e) argsOrig
   wellFormedCCs ast id args argsOrig
   let inline getLinkLocations (x, y) = 
      let xs = reb.SingleLocations(buildRegex ast reb x) |> Option.get
      let ys = reb.SingleLocations(buildRegex ast reb y) |> Option.get
      (xs, ys)
   match id.Name with
   | "aggregate" -> 
      let p = getPrefix (List.head args).Node
      let (x, y) = getLinks (List.item 1 args).Node
      let (xs, ys) = getLinkLocations (x, y)
      CAggregate(p, xs, ys)
   | "tag" -> 
      let (a, b) = getComm (List.head args).Node
      let p = getPrefix (List.item 1 args).Node
      let (x, y) = getLinks (List.item 2 args).Node
      let (xs, ys) = getLinkLocations (x, y)
      let str = (string a) + ":" + (string b)
      CCommunity(str, [ p ], xs, ys)
   | "maxroutes" -> 
      let i = getInt (List.head args).Node
      let (x, y) = getLinks (List.item 1 args).Node
      let (xs, ys) = getLinkLocations (x, y)
      CMaxRoutes(i, xs, ys)
   | "longest_path" -> 
      let i = getInt (List.head args).Node
      let r = buildRegex ast reb (List.item 1 args)
      let locs = reb.SingleLocations r
      CLongestPath(i, Option.get locs)
   | _ -> Util.unreachable()

let makeControlConstraints ast : CConstraint list = List.map (buildCConstraint ast) ast.CConstraints

(* Compiler warnings for a variety of common mistakes. 
   Unused definitions or parameters not starting with '_',
   TODO: unused aggregates, etc. *)

let inline getUsed seen e = 
   match e.Node with
   | Ident(id, _) -> seen := Set.add id.Name !seen
   | _ -> ()

let warnUnusedDefs ast e = 
   let used = ref Set.empty
   iterAllExpr ast e (getUsed used)
   let defs = Map.fold (fun acc k (p, _, _) -> Map.add k p acc) Map.empty ast.Defs
   Map.iter (fun id p -> 
      let notMain = (id <> "main")
      let notUnder = (id.[0] <> '_')
      let notUsed = (not (Set.contains id !used))
      let notRouter = (not (ast.TopoInfo.SelectGraphInfo.AsnMap.ContainsKey id))
      if notMain && notUnder && notUsed && notRouter then 
         let msg = sprintf "Unused definition of '%s'" id
         Message.warningAst ast msg p) defs

let warnUnusedParams (ast : T) = 
   Map.iter (fun id (_, ps, e) -> 
      let used = ref (Set.empty)
      iter (getUsed used) e
      for p in ps do
         let notUnder = (p.Name.[0] <> '_')
         let notUsed = (not (Set.contains p.Name !used))
         if notUnder && notUsed then 
            let msg = sprintf "Unused parameter '%s' in definition of '%s'" p.Name id
            Message.warningAst ast msg p.Pos) ast.Defs

let warnUnused (ast : T) (main : Expr) : unit = 
   warnUnusedDefs ast main
   warnUnusedParams ast

let inline getAsns asns e = 
   match e.Node with
   | Asn i -> asns := Set.add (e, i) !asns
   | _ -> ()

let warnRawAsn (ast : T) = 
   let rawAsns = ref Set.empty
   Map.iter (fun def (_, _, e) -> 
      let isRouter = 
         (ast.TopoInfo.SelectGraphInfo.InternalNames.Contains def) 
         || (ast.TopoInfo.SelectGraphInfo.ExternalNames.Contains def)
      if not isRouter then iter (getAsns rawAsns) e) ast.Defs
   for (e, asn) in !rawAsns do
      let inline isRouter r n = 
         n = asn 
         && (ast.TopoInfo.SelectGraphInfo.InternalNames.Contains r 
             || ast.TopoInfo.SelectGraphInfo.ExternalNames.Contains r)
      match Map.tryFindKey isRouter ast.TopoInfo.SelectGraphInfo.AsnMap with
      | Some router -> 
         let msg = 
            sprintf "Raw ASN literal %s used for named topology router. " (string asn) 
            + sprintf "Prefer to use the router name '%s' directly." router
         Message.warningAst ast msg e.Pos
      | None -> ()

let inline buildPrefix ast (a, b, c, d, bits) = 
   let (lo, hi) = adjustBits bits
   Prefix(a, b, c, d, lo, Range(lo, hi))

let inline getPrefixes ast pfxs e = 
   match e.Node with
   | PrefixLiteral(a, b, c, d, bits) -> 
      wellFormedPrefix ast e.Pos (a, b, c, d, bits)
      let pfx = getPrefix e.Node
      pfxs := Set.add pfx !pfxs
   | TemplateVar(_, id) when id.Name <> routerTemplate -> 
      let pfx = getPrefix e.Node
      pfxs := Set.add pfx !pfxs
   | _ -> ()

let warnUnusedAggregates (ast : T) e = 
   let inline isPfxFor p1 p2 = p1 <> p2 && Route.mightApplyTo p1 p2
   let prefixes = ref Set.empty
   iter (getPrefixes ast prefixes) e
   for (id, es) in ast.CConstraints do
      for e in es do
         let e = substitute ast e
         match e.Node with
         | PrefixLiteral(a, b, c, d, bits) -> 
            wellFormedPrefix ast e.Pos (a, b, c, d, bits)
            let pfx = getPrefix e.Node // buildPrefix ast (a,b,c,d,bits)
            if not (Set.exists (isPfxFor pfx) !prefixes) then 
               let msg = 
                  sprintf "The prefix %s does not apply to any " (string pfx) 
                  + sprintf "specific prefix used in the policy."
               Message.warningAst ast msg e.Pos
         | _ -> ()

let findTemplateViolations (ast : T) e = 
   let preds = ref Set.empty
   let regexes = ref Set.empty
   let referenced = ref Set.empty
   
   let inline add ido set = 
      match ido with
      | None -> set := Set.add "global" !set
      | Some x -> set := Set.add x.Name !set
   
   let isTemplatePred e = 
      match e.Node with
      | TemplateVar(ido, id) -> add ido preds
      | _ -> ()
   
   let isTemplateRegex e = 
      match e.Node with
      | Ident(i, [ { Node = TemplateVar(ido, id) } ]) when i.Name = "end" -> add ido regexes
      | TemplateVar(ido, _) -> add ido referenced
      | _ -> ()
   
   let aux e = 
      match e.Node with
      | BlockExpr es -> 
         for (pred, res) in es do
            preds := Set.empty
            regexes := Set.empty
            referenced := Set.empty
            iter isTemplatePred pred
            iter isTemplateRegex res
            let count = Set.count !referenced
            if count > 0 then 
               if count > 1 then 
                  let msg = 
                     sprintf "Only a single router template variable may be used, " 
                     + sprintf "and only in the end constraint"
                  Message.errorAst ast msg res.Pos
               if Set.count !regexes <> count then 
                  let msg = 
                     sprintf "Template router variables may only appear " 
                     + sprintf "directly in end constraint"
                  Message.errorAst ast msg res.Pos
               if Set.count !preds <> count then 
                  let msg = 
                     sprintf "Different number of template variables in expression: " 
                     + sprintf "left had %d, but right had %d" (Set.count !preds) 
                          (Set.count !regexes)
                  Message.errorAst ast msg (Message.range pred.Pos res.Pos)
               if !preds <> !regexes then 
                  let msg = sprintf "Non-matching template namespace on both sides of expression"
                  Message.errorAst ast msg pred.Pos
      | _ -> ()
   
   iter aux e

(* Get all the concrete locations that are originated syntactically
   at some point in the policy. This will be used to catch accidental
   Misconfigurations that originate from all locations *)

let inline getEndLocs (ast : T) locs e = 
   match e.Node with
   | Ident(id, [ e ]) when id.Name = "end" -> 
      iter (fun e' -> 
         match e'.Node with
         | Asn i -> locs := Set.add (string i) !locs
         | TemplateVar(Some id, _) -> locs := Set.add (getAsnForTemplateVar ast id) !locs
         | _ -> ()) e
   | _ -> ()

let inline originationLocations (ast : T) (e : Expr) : Set<string> = 
   let locs = ref Set.empty
   iter (getEndLocs ast locs) e
   !locs

let inline orginationLocationsList (ast : T) (es : Expr list) : Set<string> = 
   let inline addLocs acc e = Set.union acc (originationLocations ast e)
   Util.List.fold addLocs Set.empty es

(* Expand blocks in a regular expression to a
   single top-level block, and check if the 
   prefixes cover all cases. *)

type BinOp = 
   | OInter
   | OUnion
   | ODifference

let applyOp r1 r2 op = 
   match op with
   | OInter -> LAndExpr(r1, r2)
   | OUnion -> LOrExpr(r1, r2)
   | ODifference -> DiffExpr(r1, r2)

let warnDeadPrefixes ast e (pcs : (Predicate * _) list) = 
   let mutable matched = pb.False
   let mutable matchedVars = Set.empty
   for (pred, es) in pcs do
      match pred with
      | TemplatePred rs -> 
         if Set.isEmpty (Set.difference rs matchedVars) then 
            Message.warningAst ast 
               (sprintf "Dead prefix match for %s will never apply" (Route.toString pred)) e.Pos
         matchedVars <- Set.union rs matchedVars
      | ConcretePred p -> 
         let p' = pb.Or(p, matched)
         if p' = matched then 
            Message.warningAst ast 
               (sprintf "Dead prefix match for %s will never apply" (Route.toString pred)) e.Pos
         matched <- p'

let warnIncompleteBlock ast e (pcs : (Predicate * _) list) = 
   let mutable remaining = pb.True
   for (pred, es) in pcs do
      match pred with
      | TemplatePred rs -> ()
      | ConcretePred p -> remaining <- pb.And(remaining, pb.Not(pb.And(p, remaining)))
   if remaining <> pb.False then 
      let s = pb.Example remaining
      let msg = 
         sprintf 
            "Incomplete prefix matches found in block. An example of a prefix that is not matched: %s" 
            (string s)
      Message.warningAst ast msg e.Pos

let identity (op : BinOp) pos = 
   let aux n = 
      { Pos = pos
        Node = 
           Ident({ Pos = pos
                   Name = n }, []) }
   match op with
   | OUnion | ODifference -> aux "drop"
   | OInter -> aux "any"

let combineBlocks pos pcs1 pcs2 (op : BinOp) = 
   let pcs1 = pcs1 @ [ (Route.top, identity op pos) ]
   let pcs2 = pcs2 @ [ (Route.top, identity op pos) ]
   let mutable combined = []
   let mutable rollingPred = pb.False
   let mutable rollingVars = Set.empty
   let mutable lastChange = false
   for (ps, res) in pcs1 do
      for (ps', res') in pcs2 do
         let comb = Route.conj ps ps'
         
         let change = 
            match comb with
            | TemplatePred rs -> Set.isEmpty (Set.difference rs rollingVars) |> not
            | ConcretePred p -> pb.And(p, (pb.Not rollingPred)) <> pb.False
         lastChange <- change
         if change then 
            match comb with
            | TemplatePred rs -> rollingVars <- Set.union rs rollingVars
            | ConcretePred p -> rollingPred <- pb.Or(rollingPred, p)
            let both = (comb, applyOp res res' op)
            combined <- both :: combined
   let combined = 
      match combined with
      | [] -> []
      | hd :: tl -> 
         List.rev (if lastChange then tl
                   else combined)
   List.map (fun (p, n) -> 
      p, 
      { Pos = pos
        Node = n }) combined

let inline collapsePrefs pos (es : Expr list) : Expr = 
   Util.List.fold1 (fun e1 e2 -> 
      { Pos = pos
        Node = ShrExpr(e1, e2) }) es

let rec expandBlocks ast (e : Expr) : (Predicate * Expr) list = 
   let block = 
      match e.Node with
      | LOrExpr(e1, e2) -> combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) OUnion
      | LAndExpr(e1, e2) -> combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) OInter
      | DiffExpr(e1, e2) -> 
         combineBlocks e.Pos (expandBlocks ast e1) (expandBlocks ast e2) ODifference
      | BlockExpr es -> 
         List.map (fun (x, y) -> (buildPredicate x, pushPrefsToTop ast y)) es 
         |> List.map (fun (p, es) -> (p, collapsePrefs e.Pos es))
      | _ -> Util.unreachable()
   warnDeadPrefixes ast e block
   block

let addTopoDefinitions (ast : T) : T = 
   let asnDefs = 
      Map.map (fun name asn -> 
         let pos = Message.dummyPos
         let node = Asn asn
         pos, [], 
         { Pos = pos
           Node = node }) ast.TopoInfo.SelectGraphInfo.AsnMap
   
   let defs = 
      Util.Map.merge ast.Defs asnDefs (fun name ((p, _, _), _) -> 
         let msg = sprintf "Definition of '%s' clashes with router name in topology" name
         Message.errorAst ast msg p)
   
   { ast with Defs = defs }

(* Given the AST and the topology, check well-formedness
   and produce the top-level, merged path constraints. *)

type PolInfo = 
   { Ast : T
     Policy : PolicyPair list
     CConstraints : CConstraint list
     OrigLocs : Map<Predicate, Set<string>> }

let makePolicyPairs (ast : T) = 
   let ast = addTopoDefinitions ast
   match Map.tryFind "main" ast.Defs with
   | Some(_, [], e) -> 
      // Simple syntactic lints
      warnUnused ast e
      warnRawAsn ast
      // Substitute away all definitions
      let e = substitute ast e
      // Warn when templates are used in an invalid way
      findTemplateViolations ast e
      // Type check the resulting expression
      let t = wellFormed ast e
      match t with
      | BlockType -> ()
      | _ -> Message.errorAst ast (typeMsg [ BlockType ] t) e.Pos
      // Warn about aggregates that do not summarize a specific prefix
      warnUnusedAggregates ast e
      // Expand all blocks, pushing preferences to the top of the expr
      let topLevel = expandBlocks ast e |> List.map (fun (p, e) -> (p, pushPrefsToTop ast e))
      // Warn about dead prefixes/incomplete matches in top-level block
      warnIncompleteBlock ast e topLevel
      // Get all syntactically specified origination points
      let origLocs = ref Map.empty
      
      // Build pairs of (predicate, regexes)
      let polPairs = 
         List.map (fun (p : Predicate, res) -> 
            let locs = orginationLocationsList ast res
            origLocs := Map.add p locs !origLocs
            let reb = Regex.REBuilder(ast.TopoInfo.SelectGraphInfo.Graph)
            let res = List.map (buildRegex ast reb) res
            let res = List.mapi (fun i re -> reb.Build p (i + 1) re) res
            (p, reb, res)) topLevel
      (ast, polPairs, !origLocs)
   | _ -> error (sprintf "Main policy not defined, use define main = ...")

let build ast = 
   let ast, polPairs, origLocs = makePolicyPairs ast
   let ccs = makeControlConstraints ast
   { Ast = ast
     Policy = polPairs
     OrigLocs = origLocs
     CConstraints = ccs }