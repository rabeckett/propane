module Test
open IR
open CGraph
open Extension.Error

(********************************************* 
 *  Config helpers
 *********************************************)

let isPeer x rule = 
    match rule with 
    | IR.Peer y -> x = y
    | IR.State(_,y) -> x = y
    | _ -> false

let getLP (r : Rule) =
    let aux act =  match act with SetLP i -> Some i | _ -> None
    match List.tryPick aux r.Export with 
    | None -> 100
    | Some x -> x

let prefersPeer config x (a,b) =
    try 
        let rules = Map.find x config 
        let r1 = List.find (fun (rule: Rule) -> isPeer a rule.Import) rules
        let r2 = List.find (fun (rule: Rule) -> isPeer b rule.Import) rules
        let lp1 = getLP r1
        let lp2 = getLP r2
        lp1 < lp2
    with _ -> false

let receiveFrom config x y = 
    try 
        let rules = Map.find x config 
        List.exists (fun (rule: Rule) -> isPeer y rule.Import) rules
    with _ -> false

let originate (rule: Rule) = 
    List.exists (fun act -> 
        match act with 
        | Originate -> true 
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
     Explanation: string;
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

let rDatacenterSmall4 (reb: Regex.REBuilder) =
    [reb.ConcatAll [reb.Star reb.Inside; reb.Loc "A"]]

let rDatacenterSmall5 (reb: Regex.REBuilder) =
    [reb.ConcatAll [reb.Star reb.Inside; reb.Loc "M"; reb.Star reb.Inside; reb.Loc "A"]]

let rDatacenterMedium1 (reb: Regex.REBuilder) =
    [reb.Star reb.Inside]

let rDatacenterMedium2 (reb: Regex.REBuilder) =
    [reb.ConcatAll [reb.Star reb.Inside; reb.Loc "X"; reb.Star reb.Inside; reb.Loc "F"]]

let rDatacenterMedium3 (reb: Regex.REBuilder) =
    let re1 = reb.ConcatAll [reb.Star reb.Inside; reb.Loc "X"; reb.Star reb.Inside; reb.Loc "F"] 
    let re2 = reb.ConcatAll [reb.Star reb.Inside; reb.Loc "Y"; reb.Star reb.Inside; reb.Loc "F"]
    [re1; re2]

let rBrokenTriangle1 (reb: Regex.REBuilder) =
    [reb.Union 
        (reb.ConcatAll [reb.Loc "C"; reb.Loc "A"; reb.Loc "E"; reb.Loc "D"]) 
        (reb.ConcatAll [reb.Loc "A"; reb.Loc "B"; reb.Loc "D"])]


let tests = [

    {Name= "Diamond1";
     Explanation="A simple path";
     Topo= tDiamond;
     Rf= rDiamond1; 
     Receive= Some [("Y", "B"); ("N","Y"); ("X","N"); ("A","X")];
     Originate = Some ["B"];
     Prefs = Some []};

    {Name= "Diamond2";
     Explanation="Impossible Backup (should fail)";
     Topo= tDiamond;
     Rf= rDiamond2; 
     Receive= None;
     Originate = None;
     Prefs = None};

    {Name= "DCsmall1";
     Explanation="Shortest paths routing";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall1; 
     Receive= Some [];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some []};
   
    {Name= "DCsmall2";
     Explanation="Waypoint through spine";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall2; 
     Receive= Some [("M","X"); ("M","Y"); ("A","X"); ("B","X"); ("X", "A"); ("X", "B"); ("X", "M")];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some []};

    {Name= "DCsmall3";
     Explanation="Prefer one spine over another";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall3; 
     Receive= Some [("M","X"); ("M","Y"); ("N","X"); ("N","Y"); ("A","X"); 
                    ("B","X"); ("X", "A"); ("X", "B"); ("X","N"); ("X", "M")];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some [("X", "M", "N"); ("Y", "M","N")]};

    {Name= "DCsmall4";
     Explanation="End at single location";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall4; 
     Receive= Some [("X", "A"); ("M","X"); ("N", "X"); ("Y", "M"); ("Y", "N"); ("C", "Y"); ("D", "Y")];
     Originate = Some ["A"];
     Prefs = Some []};

    {Name= "DCsmall5";
     Explanation="Waypoint through spine to single location (should fail)";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall5; 
     Receive= None;
     Originate = None;
     Prefs = None};

    {Name= "DCmedium1";
     Explanation="Shortest paths routing";
     Topo= tDatacenterMedium;
     Rf= rDatacenterMedium1; 
     Receive= Some [];
     Originate = Some [];
     Prefs = Some []};

    {Name= "DCmedium2";
     Explanation="Waypoint through spine (should fail)";
     Topo= tDatacenterMedium;
     Rf= rDatacenterMedium2; 
     Receive= None;
     Originate = None;
     Prefs = None}; 

    {Name= "BrokenTriangle1";
     Explanation="Inconsistent path suffixes (should fail)";
     Topo= tBrokenTriangle;
     Rf= rBrokenTriangle1; 
     Receive= None;
     Originate = None;
     Prefs = None};
]

let run () =
    for test in tests do
        printfn "Testing %s ..." test.Name
        let reb = Regex.REBuilder test.Topo
        match IR.compileToIR test.Topo reb (test.Rf reb) (Some test.Name) with 
        | Err(_) ->
            if (Option.isSome test.Receive || 
                Option.isSome test.Originate || 
                Option.isSome test.Prefs) then 
                printfn "[Failed]:\n  Name: %s\n  Message: Should compile but did not\n" test.Name
        | Ok(config) -> 
            if (Option.isNone test.Receive || 
                Option.isNone test.Originate || 
                Option.isNone test.Prefs) then 
                printfn "[Failed]: (%s) Should not compile but did" test.Name
            else
                (* Check receiving from peers *)
                let rs = Option.get test.Receive
                for (x,y) in rs do 
                    if not (receiveFrom config x y) then 
                        printfn "[Failed]: (%s) %s should receive from %s but did not" test.Name x y
                
                (* Check originating routes *)
                let os = Option.get test.Originate
                for x in os do 
                    if not (originates config x) then 
                        printfn "[Failed]: (%s) %s should originate a route but did not" test.Name x

                (* Test preferences *)
                let ps = Option.get test.Prefs
                for (x,a,b) in ps do
                    if not (prefersPeer config x (a,b)) then 
                        printfn "[Failed]: (%s) %s should prefer %s to %s but did not" test.Name x a b

    