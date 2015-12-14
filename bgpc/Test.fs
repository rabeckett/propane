module Test
open IR
open CGraph
open System
open Common.Debug
open Common.Error

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

type FailReason = 
    | InconsistentPrefs
    | NoPathForRouters

type Test = 
    {Name: string;
     Explanation: string;
     Topo: Topology.T;
     Rf: Regex.REBuilder -> Regex.T list;
     Receive: (string*string) list option;
     Originate: string list option;
     Prefs: (string*string*string) list option
     Fail: FailReason option}
 
let tDiamond = Examples.topoDiamond () 
let tDatacenterSmall = Examples.topoDatacenterSmall ()
let tDatacenterMedium = Examples.topoDatacenterMedium ()
let tDatacenterLarge = Examples.topoDatacenterLarge ()
let tBrokenTriangle = Examples.topoBrokenTriangle ()
let tBigDipper = Examples.topoBigDipper () 
let tBadGadget = Examples.topoBadGadget ()
let tSeesaw = Examples.topoSeesaw ()

let rDiamond1 (reb: Regex.REBuilder) = 
    [reb.ConcatAll (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])]

let rDiamond2 (reb: Regex.REBuilder) = 
    let re1 = reb.ConcatAll (List.map reb.Loc ["A"; "X"; "N"; "Y"; "B"])
    let re2 = reb.ConcatAll [reb.Loc "A"; reb.Star reb.Inside; reb.Loc "N"; reb.Loc "Z"; reb.Loc "B"]
    [re1; re2]

let rDatacenterSmall1 (reb: Regex.REBuilder) = 
    [reb.Internal()]

let rDatacenterSmall2 (reb: Regex.REBuilder) = 
    [reb.InterAll [reb.Waypoint("M"); reb.EndsAt("A")]]

let rDatacenterSmall3 (reb: Regex.REBuilder) =
    let pref1 = reb.InterAll [reb.Waypoint "M"; reb.EndsAt "A"]
    let pref2 = reb.InterAll [reb.Internal(); reb.EndsAt "A"]
    [pref1; pref2]

let rDatacenterSmall4 (reb: Regex.REBuilder) =
    [reb.EndsAt("A")]

let rDatacenterSmall5 (reb: Regex.REBuilder) =
    [reb.InterAll [reb.Waypoint("M"); reb.EndsAt("A")]]

let rDatacenterMedium1 (reb: Regex.REBuilder) =
    [reb.Internal()]

let rDatacenterMedium2 (reb: Regex.REBuilder) =
    [reb.InterAll [reb.Waypoint("X"); reb.EndsAt("F")]]

let rDatacenterLarge1 (reb: Regex.REBuilder) =
    [reb.InterAll [reb.Waypoint("M"); reb.EndsAt("A")]]

let rDatacenterLarge2 (reb: Regex.REBuilder) =
    let pref1 = reb.InterAll [reb.Waypoint("M"); reb.EndsAt("A")]
    let pref2 = reb.EndsAt("A")
    [pref1; pref2]

let rDatacenterLarge3 (reb: Regex.REBuilder) =
    let pref1 = reb.InterAll [reb.Waypoint("M"); reb.EndsAt("A")]
    let pref2 = reb.InterAll [reb.Waypoint("N"); reb.EndsAt("A")]
    let pref3 = reb.EndsAt("A")
    [pref1; pref2; pref3]

let rBrokenTriangle1 (reb: Regex.REBuilder) =
    [reb.Union
        (reb.Path(["C"; "A"; "E"; "D"]))
        (reb.Path(["A"; "B"; "D"])) ]

let rBigDipper1 (reb: Regex.REBuilder) =
    let op1 = reb.Path(["C"; "A"; "E"; "D"])
    let op2 = reb.Path(["A"; "E"; "D"])
    let op3 = reb.Path(["A"; "D"])
    [reb.UnionAll [op1; op2; op3]]

let rBadGadget1 (reb: Regex.REBuilder) =
    let op1 = reb.Path(["A"; "C"; "D"])
    let op2 = reb.Path(["B"; "A"; "D"])
    let op3 = reb.Path(["C"; "B"; "D"])
    let pref1 = reb.UnionAll [op1; op2; op3]
    let op4 = reb.Path(["A"; "D"]) 
    let op5 = reb.Path(["B"; "D"])
    let op6 = reb.Path(["C"; "D"])
    let pref2 = reb.UnionAll [op4; op5; op6]
    [pref1; pref2]

let rBadGadget2 (reb: Regex.REBuilder) =
    let op1 = reb.Path(["A"; "C"; "D"])
    let op2 = reb.Path(["B"; "A"; "D"])
    let op3 = reb.Path(["C"; "B"; "D"])
    let op4 = reb.Path(["A"; "D"]) 
    let op5 = reb.Path(["B"; "D"])
    let op6 = reb.Path(["C"; "D"])
    [reb.UnionAll [op1; op2; op3; op4; op5; op6]]

let rSeesaw1 (reb: Regex.REBuilder) = 
    let op1 = reb.Path(["A"; "X"; "N"; "M"])
    let op2 = reb.Path(["B"; "X"; "N"; "M"])
    let op3 = reb.Path(["A"; "X"; "O"; "M"])
    let op4 = reb.Path(["X"; "O"; "M"])
    let pref1 = reb.UnionAll [op1; op2; op3; op4]
    let pref2 = reb.Path(["X"; "N"; "M"])
    [pref1; pref2]

let tests = [

    {Name= "Diamond1";
     Explanation="A simple path";
     Topo= tDiamond;
     Rf= rDiamond1; 
     Receive= Some [("Y", "B"); ("N","Y"); ("X","N"); ("A","X")];
     Originate = Some ["B"];
     Prefs = Some [];
     Fail = None};

    {Name= "Diamond2";
     Explanation="Impossible Backup (should fail)";
     Topo= tDiamond;
     Rf= rDiamond2; 
     Receive= None;
     Originate = None;
     Prefs = None; 
     Fail = Some InconsistentPrefs};

    {Name= "DCsmall1";
     Explanation="Shortest paths routing";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall1; 
     Receive= Some [];
     Originate = Some ["A"; "B"; "C"; "D"];
     Prefs = Some [];
     Fail = None};
   
    {Name= "DCsmall2";
     Explanation="Waypoint through spine no backup (should fail)";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall2; 
     Receive= None;
     Originate = None;
     Prefs = None; 
     Fail = Some NoPathForRouters};

    {Name= "DCsmall3";
     Explanation="Waypoint through spine with backup";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall3; 
     Receive= Some [("X", "A"); ("M", "X"); ("N", "X"); ("B", "X"); ("Y", "M"); ("Y", "N"); ("C", "Y"); ("D", "Y")];
     Originate = Some ["A"];
     Prefs = Some [("Y", "M", "N")]; 
     Fail = None};

   {Name= "DCsmall4";
     Explanation="End at single location";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall4; 
     Receive= Some [("X", "A"); ("M","X"); ("N", "X"); ("Y", "M"); ("Y", "N"); ("C", "Y"); ("D", "Y")];
     Originate = Some ["A"];
     Prefs = Some []; 
     Fail = None };

    {Name= "DCsmall5";
     Explanation="Waypoint through spine to single location (should fail)";
     Topo= tDatacenterSmall;
     Rf= rDatacenterSmall5; 
     Receive= None;
     Originate = None;
     Prefs = None; 
     Fail = Some NoPathForRouters};

    {Name= "DCmedium1";
     Explanation="Shortest paths routing";
     Topo= tDatacenterMedium;
     Rf= rDatacenterMedium1; 
     Receive= Some [];
     Originate = Some [];
     Prefs = Some [];
     Fail = None};

    {Name= "DCmedium2";
     Explanation="Waypoint through spine (should fail)";
     Topo= tDatacenterMedium;
     Rf= rDatacenterMedium2; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some InconsistentPrefs};

    {Name= "DClarge1";
     Explanation="Waypoint through spine (should fail)";
     Topo= tDatacenterLarge;
     Rf= rDatacenterLarge1; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some NoPathForRouters};

    {Name= "DClarge2";
     Explanation="Waypoint through spine with backup (should fail due to valleys)";
     Topo= tDatacenterLarge;
     Rf= rDatacenterLarge2; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some InconsistentPrefs};

    {Name= "DClarge3";
     Explanation="Waypoint through spines with preference and backup (should fail due to valleys)";
     Topo= tDatacenterLarge;
     Rf= rDatacenterLarge3; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some InconsistentPrefs};

    {Name= "BrokenTriangle1";
     Explanation="Inconsistent path suffixes (should fail)";
     Topo= tBrokenTriangle;
     Rf= rBrokenTriangle1; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some NoPathForRouters};

    {Name= "BigDipper1";
     Explanation="Must choose the correct preference";
     Topo= tBigDipper;
     Rf= rBigDipper1; 
     Receive= Some [("E", "D"); ("A", "E"); ("C", "A")];
     Originate = Some ["D"];
     Prefs = Some [("A","E","D")];
     Fail = None};

    {Name= "BadGadget1";
     Explanation="Total ordering prevents instability (should fail)";
     Topo= tBadGadget;
     Rf= rBadGadget1; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some InconsistentPrefs};

    {Name= "BadGadget2";
     Explanation="Must find correct total ordering";
     Topo= tBadGadget;
     Rf= rBadGadget2; 
     Receive= Some [("A", "D"); ("B", "D"); ("C", "D")];
     Originate = Some ["D"];
     Prefs = Some [("A", "D", "C"); ("B", "D", "A"); ("C", "D", "B")];
     Fail = None};

    {Name= "Seesaw1";
     Explanation="Must get all best preferences (should fail)";
     Topo= tSeesaw;
     Rf= rSeesaw1; 
     Receive= None;
     Originate = None;
     Prefs = None;
     Fail = Some InconsistentPrefs};

]

let run debug_dir =
    for test in tests do
        let msg = String.Format("Testing {0} - {1} ...", test.Name, test.Explanation)
        printfn "%s" msg
        logInfo0("\n" + msg)
        let reb = Regex.REBuilder test.Topo
        match IR.compileToIR test.Topo reb (test.Rf reb) (Some (debug_dir + test.Name)) with 
        | Err(x) ->
            if (Option.isSome test.Receive || 
                Option.isSome test.Originate || 
                Option.isSome test.Prefs || 
                Option.isNone test.Fail) then 
                let msg = String.Format("\n[Failed]:\n  Name: {0}\n  Message: Should compile but did not\nError: {1}\n", test.Name, x)
                printfn "%s" msg
                logInfo0(msg)
            match test.Fail, x with 
            | Some NoPathForRouters, IR.NoPathForRouters _ -> ()
            | Some InconsistentPrefs, IR.InconsistentPrefs _ -> ()
            | _ ->
                let msg = String.Format("\n[Failed]:\n  Name: {0}\n  Message: Expected Error {1}\n", test.Name, test.Fail)
                printfn "%s" msg
                logInfo0(msg)
        | Ok(config) -> 
            if (Option.isNone test.Receive || 
                Option.isNone test.Originate || 
                Option.isNone test.Prefs || 
                Option.isSome test.Fail) then
                let msg = String.Format("\n[Failed]:\n  Name: {0}\n  Message: Should not compile but did\n", test.Name)
                printfn "%s" msg
                logInfo0(msg)
            else
                (* Check receiving from peers *)
                let rs = Option.get test.Receive
                for (x,y) in rs do 
                    if not (receiveFrom config x y) then
                        let msg = String.Format("\n[Failed]: ({0}) - {1} should receive from {2} but did not\n", test.Name, x, y)
                        printfn "%s" msg
                        logInfo0(msg)
                
                (* Check originating routes *)
                let os = Option.get test.Originate
                for x in os do 
                    if not (originates config x) then 
                        let msg = String.Format("\n[Failed]: ({0}) - {1} should originate a route but did not", test.Name, x)
                        printfn "%s" msg
                        logInfo0(msg)

                (* Test preferences *)
                let ps = Option.get test.Prefs
                for (x,a,b) in ps do
                    if not (prefersPeer config x (a,b)) then 
                        let msg = String.Format("\n[Failed]: ({0}) - {1} should prefer {2} to {3} but did not", test.Name, x, a, b)
                        printfn "%s" msg
                        logInfo0(msg)

    