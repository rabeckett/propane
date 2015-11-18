module Test

open Extension.Error

let canImplement cg =
    isOk (Config.compile cg)


(********************************************* 
 *  Config helpers
 *********************************************)

let isPeer x rule = 
    match rule with 
    | Config.Peer y -> x = y
    | Config.State(_,y) -> x = y
    | _ -> false

let getLP (r : Config.Rule) =
    let aux act =  match act with Config.SetLP i -> Some i | _ -> None
    match List.tryPick aux r.Export with 
    | None -> 100
    | Some x -> x

let prefersPeer config x (a,b) =
    try 
        let rules = Map.find x config 
        let r1 = List.find (fun (rule: Config.Rule) -> isPeer a rule.Import) rules
        let r2 = List.find (fun (rule: Config.Rule) -> isPeer b rule.Import) rules
        let lp1 = getLP r1
        let lp2 = getLP r2
        lp1 < lp2
    with _ -> false

let receiveFrom config x y = 
    try 
        let rules = Map.find x config 
        List.exists (fun (rule: Config.Rule) -> isPeer y rule.Import) rules
    with _ -> false

let originate (rule: Config.Rule) = 
    List.exists (fun act -> 
        match act with 
        | Config.Originate -> true 
        | _ -> false
    ) rule.Export

let originates config x =
    try
        Map.find x config
        |> List.exists originate 
    with _ -> false



(********************************************* 
 *  Regular expression queries
 *********************************************)

type Test = 
    {Name: string;
     Topo: Topology.T;
     Rf: Regex.REBuilder -> Regex.T list;
     Receive: (string*string) list option;
     Originate: string list option;
     Prefs: (string*string*string) list option}
 
let tDiamond = Examples.topoDiamond () 
let tDatacenterSmall = Examples.topoDatacenterSmall ()
let tDatacenterMedium = Examples.topoDatacenterMedium () 
let tBrokenTriangle = Examples.topoBrokenTriangle ()
let tBigDipper = Examples.topoBigDipper () 
let tBadGadget = Examples.topoBadGadget ()

let rDiamond1 (reb: Regex.REBuilder) = 
    [reb.ConcatAll (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])]

let rDiamond2 (reb: Regex.REBuilder) = 
    let re1 = reb.ConcatAll (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])
    let re2 = reb.ConcatAll [reb.Loc "A"; reb.Star reb.Inside; reb.Loc "N"; reb.Loc "Z"; reb.Loc "B"]
    [re1; re2]

let rDatacenterSmall1 (reb: Regex.REBuilder) = 
    [reb.Star reb.Inside]

let rDatacenterSmall2 (reb: Regex.REBuilder) = 
    [reb.ConcatAll [reb.Star reb.Inside; reb.Loc "M"; reb.Star reb.Inside]]

let rDatacenterSmall3 (reb: Regex.REBuilder) =
    let re1 = reb.ConcatAll [reb.Star reb.Inside; reb.Loc "M"; reb.Star reb.Inside] 
    let re2 = reb.ConcatAll [reb.Star reb.Inside; reb.Loc "N"; reb.Star reb.Inside]
    [re1; re2]

let rDatacenterMedium1 (reb: Regex.REBuilder) =
    [reb.Star reb.Inside]

let tests = [

    {Name= "Diamond1";
     Topo= tDiamond;
     Rf= rDiamond1; 
     Receive= Some [("Y", "B"); ("N","Y"); ("X","N"); ("A","X")];
     Originate = Some ["B"];
     Prefs = Some []};

    {Name= "Diamond2";
     Topo= tDiamond;
     Rf= rDiamond2; 
     Receive= None;
     Originate = None;
     Prefs = None};

    {Name= "DCsmall1";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall1; 
     Receive= Some [];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some []};
   
    {Name= "DCsmall2";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall2; 
     Receive= Some [("M","X"); ("M","Y"); ("A","X"); ("B","X"); ("X", "A"); ("X", "B"); ("X", "M")];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some []};

    {Name= "DCsmall3";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall3; 
     Receive= Some [("M","X"); ("M","Y"); ("N","X"); ("N","Y"); ("A","X"); 
                    ("B","X"); ("X", "A"); ("X", "B"); ("X","N"); ("X", "M")];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some [("X", "M", "N"); ("Y", "M","N")]};

    {Name= "DCmedium1";
     Topo= tDatacenterMedium;
     Rf= rDatacenterSmall3; 
     Receive= Some [];
     Originate = Some [];
     Prefs = Some []};

]

let run () =
    let mutable ntests = 0
    let mutable nfailed = 0
    for test in tests do
        let reb = Regex.REBuilder test.Topo
        let cg = CGraph.buildFromRegex test.Topo reb (test.Rf reb)
        Minimize.pruneHeuristic cg
        System.IO.File.WriteAllText(test.Name + ".dot", CGraph.toDot cg)
        match Config.compile cg with 
        | Err _ -> 
            ntests <- ntests + 1
            if (Option.isSome test.Receive || 
                Option.isSome test.Originate || 
                Option.isSome test.Prefs) then 
                nfailed <- nfailed + 1
                printfn "[Failed]: (%s) Should compile but did not" test.Name
        | Ok config -> 
            System.IO.File.WriteAllText(test.Name + ".config", Config.format config)
            if Option.isNone test.Receive || Option.isNone test.Originate || Option.isNone test.Prefs then 
                nfailed <- nfailed + 1
                printfn "[Failed]: (%s) Should not compile but did" test.Name
                System.IO.File.WriteAllText(test.Name + ".config", Config.format config)
            else
                (* Check receiving from peers *)
                let rs = Option.get test.Receive
                for (x,y) in rs do 
                    ntests <- ntests + 1
                    if not (receiveFrom config x y) then 
                        nfailed <- nfailed + 1
                        printfn "[Failed]: (%s) %s should receive from %s but did not" test.Name x y
                
                (* Check originating routes *)
                let os = Option.get test.Originate
                for x in os do 
                    ntests <- ntests + 1
                    if not (originates config x) then 
                        nfailed <- nfailed + 1 
                        printfn "[Failed]: (%s) %s should originate a route but did not" test.Name x

                (* Test preferences *)
                let ps = Option.get test.Prefs
                for (x,a,b) in ps do
                    ntests <- ntests + 1
                    if not (prefersPeer config x (a,b)) then 
                        nfailed <- nfailed + 1
                        printfn "[Failed]: (%s) %s should prefer %s to %s but did not" test.Name x a b


    printfn "Tests: (%d/%d) successful" (ntests-nfailed) ntests

    