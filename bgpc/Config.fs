module Config
open Extension.Error
open CGraph

type Match = 
    | Peer of string 
    | State of int array * string
    | PathRE of Regex.T
    | NoMatch

    override this.ToString () = 
        match this with 
        | Peer s -> "Peer=" + s
        | State(is,s) -> "Community=" + (List.ofArray is).ToString() + "," + s
        | PathRE r -> "Regex=" + r.ToString()
        | NoMatch -> "--"

type Action = 
    | NoAction
    | SetComm of int array * string
    | SetMed of int
    | SetLP of int
    | Originate

    override this.ToString() = 
        match this with 
        | NoAction -> ""
        | SetComm(is,s) -> "Community<-" + is.ToString() + "," + s
        | SetMed i -> "MED<-" + i.ToString()
        | SetLP i -> "LP<-" + i.ToString()
        | Originate -> "Originate"

type Actions = Action list

type Rule =
    {Import: Match;
     Export: Actions}

type T = Map<string, Rule list>

let format (config: T) = 
    let sb = System.Text.StringBuilder ()
    for kv in config do 
        sb.Append("Router ") |> ignore
        sb.Append(kv.Key) |> ignore
        for rule in kv.Value do
            sb.Append("\n  Match: ") |> ignore
            sb.Append(rule.Import.ToString()) |> ignore
            sb.Append("\n    Export: ") |> ignore
            sb.Append(rule.Export.ToString()) |> ignore
        sb.Append("\n\n") |> ignore
    sb.ToString()

let compareLocThenPref (x,i1) (y,i2) = 
    let cmp = compare i1 i2
    if cmp = 0 then
        compare x.Node.Loc y.Node.Loc
    else cmp

let rec removeAdjacentLocs sorted = 
    match sorted with 
    | [] | [_] -> sorted
    | hd1::((hd2::z) as tl) ->
        let (x,i1) = hd1 
        let (y,i2) = hd2 
        if x.Node.Loc = y.Node.Loc then removeAdjacentLocs (hd1::z)
        else hd1 :: (removeAdjacentLocs tl)

let private genConfig (cg: CGraph.T) (ord: Consistency.Ordering) : T =
    let cgRev = copyReverseGraph cg
    let neighborsIn v = seq {for e in cgRev.Graph.OutEdges v do yield e.Target}
    let neighborsOut v = seq {for e in cg.Graph.OutEdges v do yield e.Target}
    let mutable config = Map.empty
    for entry in ord do 
        let mutable rules = []
        let loc = entry.Key
        let prefs = entry.Value 
        let prefNeighborsIn =
            prefs
            |> Seq.mapi (fun i v -> (neighborsIn v, i))
            |> Seq.map (fun (ns,i) -> Seq.map (fun n -> (n,i)) ns) 
            |> Seq.fold Seq.append Seq.empty 
            |> List.ofSeq
            |> List.sortWith compareLocThenPref
            |> removeAdjacentLocs
        let mutable lp = 99
        let mutable lastPref = None
        for v, pref in prefNeighborsIn do 
            match lastPref with 
            | Some p when pref = p -> () 
            | _ ->
                lastPref <- Some pref 
                lp <- lp + 1
            let unambiguous = 
                prefNeighborsIn 
                |> Set.ofList 
                |> Set.filter (fun (x,_) -> x.Node.Loc = v.Node.Loc) 
                |> Set.count 
                |> ((=) 1)
            let m =
                match v.Node.Typ with 
                | Topology.Start -> NoMatch
                | _ -> 
                    if unambiguous then Peer v.Node.Loc 
                    else State (v.States, v.Node.Loc)
            let a = 
                match v.Node.Typ with 
                | Topology.Start -> [Originate]
                | _ ->
                    if lp = 100 then [] 
                    else [SetLP(lp)]
            rules <- {Import = m; Export = a}::rules
        config <- Map.add loc rules config
    config

let compile (cg: CGraph.T) : Result<T, Consistency.CounterExample> =
    match Consistency.findOrdering cg with 
    | Ok ord -> Ok (genConfig cg ord)
    | Err(x) -> Err(x)


(* Generate templates 
let generateTemplates (config: T) (path: string) = 
    failwith "todo" *)
